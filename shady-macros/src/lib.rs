extern crate lazy_static;
extern crate proc_macro;
extern crate quote;

use lazy_static::lazy_static;

use proc_macro::*;
use quote::quote;
use std::sync::Mutex;
use syn::{parse_macro_input, AttributeArgs, ItemFn, NestedMeta};

lazy_static! {
    static ref ALL_BUILTINS: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref ALL_EVAL_BUILTINS: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

struct Builtin {
    fn_name: String,
    is_infix: bool,
}

impl Builtin {
    fn new(orig_fun: &ItemFn, macro_args: &Vec<NestedMeta>) -> Self {
        let mut fn_name = orig_fun.sig.ident.to_string();
        let mut is_infix = false;

        for arg in macro_args {
            match arg {
                syn::NestedMeta::Meta(m) => match m {
                    syn::Meta::NameValue(nv) => {
                        let name = nv.path.get_ident().unwrap().to_string();
                        let value = match &nv.lit {
                            syn::Lit::Bool(b) => b.value(),
                            _ => panic!("Unsupported literal type"),
                        };
                        match name.as_str() {
                            "infix" => is_infix = value,
                            _ => panic!("Unsupported attribute"),
                        }
                    }
                    _ => {}
                },
                syn::NestedMeta::Lit(l) => match l {
                    syn::Lit::Str(s) => fn_name = s.value(),
                    _ => {}
                },
            }
        }

        Builtin {
            fn_name,
            is_infix,
        }
    }
}

#[proc_macro_attribute]
pub fn builtin(args: TokenStream, input: TokenStream) -> TokenStream {
    let orig_fun = parse_macro_input!(input as ItemFn);
    let macro_args = parse_macro_input!(args as AttributeArgs);

    let orig_fun_ident = &orig_fun.sig.ident;

    let mut params_prog = quote! {};
    let mut args_prog = quote! {};
    let mut call_prog = quote! {};
    for (i, param) in orig_fun.sig.inputs.clone().into_iter().enumerate() {
        match param {
            syn::FnArg::Typed(typed) => {
                let ident = match typed.pat.as_ref() {
                    syn::Pat::Ident(orig_fun_ident) => orig_fun_ident.ident.clone(),
                    _ => panic!("Invalid parameter"),
                };
                let param_name = ident.to_string();
                let ty = &typed.ty;

                params_prog.extend(quote! {
                    crate::ast::Parameter {
                        name: #param_name.to_string(),
                        typ: crate::types::value_type::<#ty>(),
                        spec: crate::ast::ParamSpec::default(),
                    },
                });

                // Don't clone Proc values to avoid keeping pipe handles alive
                let from_value_call = if quote!(#ty).to_string().contains("Proc") {
                    quote! {
                        crate::types::from_value::<#ty>(
                            std::mem::replace(&mut args[#i], crate::types::Value::Int(0))
                        )?
                    }
                } else {
                    quote! { crate::types::from_value::<#ty>(args[#i].clone())? }
                };

                args_prog.extend(quote! {
                    let #ident = #from_value_call;
                });

                call_prog.extend(quote! { #ident, });
            }
            _ => panic!("Invalid parameter"),
        }
    }

    let setup_fname = format!("setup_{}_builtin", orig_fun_ident);
    ALL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .push(setup_fname.clone());

    let builtin = Builtin::new(&orig_fun, &macro_args);
    let (fn_name, is_infix) = (builtin.fn_name, builtin.is_infix);
    let setup_ident = syn::Ident::new(&setup_fname, orig_fun_ident.span());
    let return_type = match orig_fun.sig.output {
        syn::ReturnType::Type(_, ref ty) => &**ty,
        _ => panic!("Invalid return type"),
    };

    // Helper function to extract inner type from Result<T>
    fn extract_result_inner_type(ty: &syn::Type) -> Option<proc_macro2::TokenStream> {
        // Guard: must be a path type
        let type_path = match ty {
            syn::Type::Path(tp) => tp,
            _ => return None,
        };

        // Guard: must have at least one segment
        let segment = type_path.path.segments.last()?;

        // Guard: type name must be "Result"
        if segment.ident != "Result" {
            return None;
        }

        // Guard: must have angle-bracketed arguments (e.g., Result<T>)
        let args = match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => args,
            _ => return None,
        };

        // Guard: must have at least one type argument
        let inner = match args.args.first()? {
            syn::GenericArgument::Type(t) => t,
            _ => return None,
        };

        Some(quote!(#inner))
    }

    // Determine if this is Result<T> and extract inner type
    let (value_type_arg, result_conversion) = if let Some(inner_type) = extract_result_inner_type(return_type) {
        // Function returns Result<T> - use ? to propagate errors
        (
            inner_type,
            quote! {
                let r = fun(#call_prog)?;
                Ok(crate::types::to_value(r))
            }
        )
    } else {
        // Function returns T directly - wrap in Ok
        (
            quote!(#return_type),
            quote! {
                let r = fun(#call_prog);
                Ok(crate::types::to_value(r))
            }
        )
    };

    quote! {
        #orig_fun

        #[allow(clippy::mutable_key_type)]
        pub fn #setup_ident(builtins: &mut crate::eval::BuiltinIndex) {
            let fun = #orig_fun_ident;
            let signature = crate::ast::FnSignature {
                fn_name: #fn_name.to_string(),
                parameters: vec![
                    #params_prog
                ],
                is_public: true,
                is_infix: #is_infix,
                return_type: crate::types::value_type::<#value_type_arg>(),
            };
            builtins.insert(
                signature,
                crate::eval::Builtin::Pure(
                    Box::new(move |mut args| -> crate::error::Result<crate::types::Value> {
                        #args_prog
                        #result_conversion
                    })
                ),
            );
        }
    }
    .into()
}

/// Extract type specs from function signature by inspecting generic marker types
fn infer_types_from_signature(sig: &syn::Signature) -> (String, String) {
    let mut param_specs = Vec::new();

    // Extract parameter types (skip LocalContext and ShadyContext)
    for input in &sig.inputs {
        if let syn::FnArg::Typed(typed) = input {
            let type_name = get_type_ident(&typed.ty);

            // Skip the context parameters
            if type_name == "LocalContext" || type_name == "ShadyContext" {
                continue;
            }

            // Convert type to spec (will default to "any" for unknown types)
            let spec = type_to_spec(&typed.ty);
            param_specs.push(spec);
        }
    }

    // Extract return type
    let return_spec = match &sig.output {
        syn::ReturnType::Type(_, ty) => {
            // Handle Result<T> wrapper
            if let Some(inner) = extract_result_type(ty) {
                type_to_spec(inner)
            } else {
                type_to_spec(ty)
            }
        }
        syn::ReturnType::Default => "any".to_string(),
    };

    (param_specs.join(", "), return_spec)
}

/// Extract the inner type from Result<T>
fn extract_result_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(type_path) = ty {
        let segment = type_path.path.segments.last()?;
        if segment.ident == "Result" {
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                    return Some(inner);
                }
            }
        }
    }
    None
}

/// Get the identifier name of a type (e.g., "Lambda1", "List", "Value")
fn get_type_ident(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Reference(type_ref) => get_type_ident(&type_ref.elem),
        syn::Type::Path(type_path) => {
            type_path.path.segments.last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default()
        }
        _ => String::new(),
    }
}

/// Rewrite a type from marker types to runtime types
/// Examples:
/// - &Lambda1<T, R> → &Lambda
/// - &Lambda2<T1, T2, R> → &Lambda
/// - &List<T> → &Value
/// - &Value → &Value (unchanged)
/// - Generic type parameters (T, R, Acc, etc.) → Value
fn rewrite_type_to_runtime(ty: &syn::Type) -> syn::Type {
    match ty {
        syn::Type::Reference(type_ref) => {
            let mut new_ref = type_ref.clone();
            new_ref.elem = Box::new(rewrite_type_to_runtime(&type_ref.elem));
            syn::Type::Reference(new_ref)
        }
        syn::Type::Path(type_path) => {
            // Check if this is a single-segment path (likely a generic type parameter)
            if type_path.path.segments.len() == 1 {
                let segment = &type_path.path.segments[0];
                let ident = segment.ident.to_string();

                // Check if it has no generic arguments (indicating a simple type parameter)
                let is_generic_param = matches!(segment.arguments, syn::PathArguments::None);

                match ident.as_str() {
                    "LambdaType" => {
                        // Replace with Lambda
                        syn::parse_quote!(crate::types::Lambda)
                    }
                    "List" => {
                        // Replace with Value
                        syn::parse_quote!(crate::types::Value)
                    }
                    // Known marker types
                    "Any" | "Int" | "Str" | "Bool" => ty.clone(),
                    // Known runtime types
                    "Value" | "Lambda" | "LocalContext" | "ShadyContext" => ty.clone(),
                    // Everything else without arguments is likely a generic parameter -> Value
                    _ if is_generic_param => {
                        syn::parse_quote!(crate::types::Value)
                    }
                    _ => ty.clone(),
                }
            } else {
                ty.clone()
            }
        }
        _ => ty.clone(),
    }
}

/// Rewrite function signature to strip generics and replace marker types with runtime types
fn rewrite_signature_for_runtime(mut sig: syn::Signature) -> syn::Signature {
    // Remove generic parameters
    sig.generics = syn::Generics::default();

    // Rewrite parameter types
    for input in sig.inputs.iter_mut() {
        if let syn::FnArg::Typed(typed) = input {
            typed.ty = Box::new(rewrite_type_to_runtime(&typed.ty));
        }
    }

    // Rewrite return type
    if let syn::ReturnType::Type(arrow, ty) = &mut sig.output {
        // Check if return type is Result<T> and rewrite the inner type
        if let syn::Type::Path(type_path) = ty.as_ref() {
            if let Some(segment) = type_path.path.segments.last() {
                if segment.ident == "Result" {
                    // Result<T> - rewrite to Result<Value>
                    *ty = Box::new(syn::parse_quote!(crate::error::Result<crate::types::Value>));
                    return sig;
                }
            }
        }
        // Otherwise just rewrite the type normally
        *ty = Box::new(rewrite_type_to_runtime(ty));
    }

    sig
}

/// Convert a Rust type to a Shady type spec string
/// Examples:
/// - &LambdaType<(Any,), Bool> → "fn(any)->bool"
/// - &LambdaType<(Int, Str), Bool> → "fn(int,str)->bool"
/// - &List<Int> → "[int]"
/// - &Value → "any"
/// - Unknown types → "any"
fn type_to_spec(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Reference(type_ref) => {
            type_to_spec(&type_ref.elem)
        }
        syn::Type::Path(type_path) => {
            let Some(segment) = type_path.path.segments.last() else {
                return "any".to_string();
            };
            let ident = segment.ident.to_string();

            match ident.as_str() {
                // Marker types
                "Any" => "any".to_string(),
                "Int" => "int".to_string(),
                "Str" => "str".to_string(),
                "Bool" => "bool".to_string(),

                // List<T>
                "List" => {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            let inner_spec = type_to_spec(inner);
                            return format!("[{}]", inner_spec);
                        }
                    }
                    "[any]".to_string()
                }

                // LambdaType<Params, R> where Params is a tuple
                "LambdaType" => {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        let args_vec: Vec<_> = args.args.iter().collect();
                        if args_vec.len() == 2 {
                            if let (
                                syn::GenericArgument::Type(params_ty),
                                syn::GenericArgument::Type(ret_ty),
                            ) = (args_vec[0], args_vec[1]) {
                                // Extract parameter types from tuple
                                let param_specs = extract_tuple_types(params_ty);
                                let ret_spec = type_to_spec(ret_ty);
                                return format!("fn({})->{}", param_specs.join(","), ret_spec);
                            }
                        }
                    }
                    "fn(any)->any".to_string()
                }

                // Runtime types (fallback to "any")
                "Value" | "Lambda" => "any".to_string(),

                // Unknown types default to "any"
                _ => "any".to_string(),
            }
        }
        _ => "any".to_string(),
    }
}

/// Extract types from a tuple type like (Int, Str, Bool)
fn extract_tuple_types(ty: &syn::Type) -> Vec<String> {
    match ty {
        syn::Type::Tuple(tuple) => {
            tuple.elems.iter().map(|elem| type_to_spec(elem)).collect()
        }
        _ => {
            // Not a tuple, treat as single parameter
            vec![type_to_spec(ty)]
        }
    }
}

/// Macro for Eval builtins (special forms) that need access to unevaluated expressions
///
/// NEW USAGE (type-inferred):
/// ```
/// #[eval_builtin]
/// pub fn map_impl<T, R>(
///     lambda: &LambdaType<(T,), R>,
///     list: &List<T>,
///     local_context: &LocalContext,
///     context: &ShadyContext,
/// ) -> Result<List<R>>
/// ```
/// The macro inspects generic parameters to automatically generate type specs.
/// Use tuple syntax for lambda parameters: LambdaType<(P1, P2), R> for multi-param lambdas.
///
/// OLD USAGE (still supported):
/// ```
/// #[eval_builtin("name", "param_types", "return_type")]
/// ```
#[proc_macro_attribute]
pub fn eval_builtin(args: TokenStream, input: TokenStream) -> TokenStream {
    let orig_fun = parse_macro_input!(input as ItemFn);
    let macro_args = parse_macro_input!(args as AttributeArgs);

    let orig_fun_ident = &orig_fun.sig.ident;

    // Check if using old-style (with string specs) or new-style (type-inferred)
    let use_type_inference = macro_args.is_empty() || macro_args.len() == 1;

    let (fn_name, param_spec, return_spec) = if use_type_inference {
        // New style: extract from function signature
        let fn_name = if macro_args.len() == 1 {
            if let syn::NestedMeta::Lit(syn::Lit::Str(s)) = &macro_args[0] {
                s.value()
            } else {
                orig_fun_ident.to_string()
            }
        } else {
            orig_fun_ident.to_string()
        };

        let (params, ret) = infer_types_from_signature(&orig_fun.sig);
        (fn_name, params, ret)
    } else {
        // Old style: parse macro arguments
        let mut fn_name = orig_fun_ident.to_string();
        let mut param_spec = String::new();
        let mut return_spec = String::new();

        for (i, arg) in macro_args.iter().enumerate() {
            if let syn::NestedMeta::Lit(syn::Lit::Str(s)) = arg {
                match i {
                    0 => fn_name = s.value(),
                    1 => param_spec = s.value(),
                    2 => return_spec = s.value(),
                    _ => {}
                }
            }
        }
        (fn_name, param_spec, return_spec)
    };

    let setup_fname = format!("setup_{}_builtin", orig_fun_ident);
    ALL_EVAL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .push(setup_fname.clone());

    let setup_ident = syn::Ident::new(&setup_fname, orig_fun_ident.span());

    // Parse parameter specs like "fn(any)->any, [any]"
    // Need to be careful with commas inside function types
    let param_specs = parse_param_specs(&param_spec);

    let mut params_prog = quote! {};
    for (i, spec) in param_specs.iter().enumerate() {
        let param_name = format!("arg{}", i);
        let param_type_code = parse_type_spec(spec);

        params_prog.extend(quote! {
            crate::ast::Parameter {
                name: #param_name.to_string(),
                typ: #param_type_code,
                spec: crate::ast::ParamSpec::default(),
            },
        });
    }

    let return_type_code = parse_type_spec(&return_spec);

    // Generate validation wrapper that checks arguments before calling the impl
    let num_params = param_specs.len();
    let wrapper_ident = syn::Ident::new(&format!("{}_wrapper", orig_fun_ident), orig_fun_ident.span());

    // Generate lambda validation code for function types
    let mut lambda_validations = quote! {};
    for (i, spec) in param_specs.iter().enumerate() {
        if let Some(validation) = generate_lambda_validation(spec, i, &fn_name) {
            lambda_validations.extend(validation);
        }
    }

    // Generate parameter extraction code
    let mut param_extractions = quote! {};
    let mut param_names = Vec::new();
    for (i, spec) in param_specs.iter().enumerate() {
        let param_ident = syn::Ident::new(&format!("arg{}", i), orig_fun_ident.span());
        param_names.push(param_ident.clone());

        // If this is a lambda parameter, extract it; otherwise just reference the Value
        if spec.trim().starts_with("fn(") {
            param_extractions.extend(quote! {
                let #param_ident = match &args[#i] {
                    crate::types::Value::Lambda(l) => l,
                    _ => unreachable!("Lambda validation should have been done"),
                };
            });
        } else {
            param_extractions.extend(quote! {
                let #param_ident = &args[#i];
            });
        }
    }

    // If using type inference, rewrite the function signature to use runtime types
    let output_fun = if use_type_inference {
        let mut rewritten_fun = orig_fun.clone();
        rewritten_fun.sig = rewrite_signature_for_runtime(rewritten_fun.sig);
        quote! { #rewritten_fun }
    } else {
        quote! { #orig_fun }
    };

    quote! {
        #output_fun

        // Generate a wrapper that validates arguments
        fn #wrapper_ident(
            args: Vec<crate::types::Value>,
            local_context: &crate::eval::LocalContext,
            context: &crate::eval::ShadyContext,
        ) -> crate::error::Result<crate::types::Value> {
            // Check argument count
            if args.len() != #num_params {
                return Err(crate::error::ShadyError::EvalError(format!(
                    "{} requires exactly {} argument{}, got {}",
                    #fn_name,
                    #num_params,
                    if #num_params == 1 { "" } else { "s" },
                    args.len()
                )));
            }

            #lambda_validations

            // Extract parameters
            #param_extractions

            // Call the actual implementation
            #orig_fun_ident(#(#param_names,)* local_context, context)
        }

        #[allow(clippy::mutable_key_type)]
        pub fn #setup_ident(builtins: &mut crate::eval::BuiltinIndex) {
            let signature = crate::ast::FnSignature {
                fn_name: #fn_name.to_string(),
                parameters: vec![
                    #params_prog
                ],
                is_public: true,
                is_infix: false,
                return_type: #return_type_code,
            };
            builtins.insert(
                signature,
                crate::eval::Builtin::Eval(Box::new(#wrapper_ident)),
            );
        }
    }
    .into()
}

/// Parse parameter specs, being careful about commas inside function types
/// E.g., "fn(any,any)->any, any, [any]" should split into ["fn(any,any)->any", "any", "[any]"]
fn parse_param_specs(spec: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut depth = 0; // Track nesting depth of parentheses/brackets

    for ch in spec.chars() {
        match ch {
            '(' | '[' => {
                depth += 1;
                current.push(ch);
            }
            ')' | ']' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                // Top-level comma - this separates parameters
                if !current.trim().is_empty() {
                    result.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }

    // Don't forget the last parameter
    if !current.trim().is_empty() {
        result.push(current.trim().to_string());
    }

    result
}

/// Generate validation code for lambda parameters
/// Returns validation code if the spec is a function type, None otherwise
fn generate_lambda_validation(spec: &str, arg_index: usize, fn_name: &str) -> Option<proc_macro2::TokenStream> {
    let spec = spec.trim();

    // Check if this is a function type
    if !spec.starts_with("fn(") {
        return None;
    }

    let arrow_pos = spec.find(")->")?;
    let params_str = &spec[3..arrow_pos];
    let return_str = &spec[arrow_pos+3..];

    // Count parameters
    let expected_param_count = if params_str.is_empty() {
        0
    } else {
        params_str.split(',').count()
    };

    // Parse return type for validation
    let return_type_check = if return_str.trim() == "bool" {
        quote! {
            // Check that lambda returns bool
            if lambda.return_type != crate::types::Type::Bool && lambda.return_type != crate::types::Type::Any {
                return Err(crate::error::ShadyError::TypeMismatch {
                    expected: "bool".to_string(),
                    actual: format!("{}", lambda.return_type),
                    span: miette::SourceSpan::from(0..0),
                });
            }
        }
    } else {
        quote! {} // No specific return type validation for other types
    };

    Some(quote! {
        // Validate lambda parameter for argument #arg_index
        {
            let lambda = match &args[#arg_index] {
                crate::types::Value::Lambda(l) => l,
                val => {
                    return Err(crate::error::ShadyError::TypeMismatch {
                        expected: "lambda function".to_string(),
                        actual: format!("{}", val.get_type()),
                        span: miette::SourceSpan::from(0..0),
                    });
                }
            };

            // Check parameter count
            if lambda.parameters.len() != #expected_param_count {
                return Err(crate::error::ShadyError::FunctionSignatureMismatch {
                    name: #fn_name.to_string(),
                    arg_types: format!(
                        "{} requires a lambda with {} parameter{}, got {}",
                        #fn_name,
                        #expected_param_count,
                        if #expected_param_count == 1 { "" } else { "s" },
                        lambda.parameters.len()
                    ),
                    span: miette::SourceSpan::from(0..0),
                });
            }

            #return_type_check
        }
    })
}

/// Parse a type spec string like "int", "[any]", "fn(any)->bool" into Type code
fn parse_type_spec(spec: &str) -> proc_macro2::TokenStream {
    let spec = spec.trim();

    // Check for list type [T]
    if spec.starts_with('[') && spec.ends_with(']') {
        let inner = &spec[1..spec.len()-1];
        let inner_type = parse_type_spec(inner);
        return quote! { crate::types::Type::List(Box::new(#inner_type)) };
    }

    // Check for function type fn(T1, T2, ...) -> R
    if spec.starts_with("fn(") {
        if let Some(arrow_pos) = spec.find(")->") {
            let params_str = &spec[3..arrow_pos];
            let return_str = &spec[arrow_pos+3..];

            let param_types: Vec<proc_macro2::TokenStream> = if params_str.is_empty() {
                vec![]
            } else {
                params_str.split(',')
                    .map(|s| parse_type_spec(s.trim()))
                    .collect()
            };

            let return_type = parse_type_spec(return_str.trim());

            return quote! {
                crate::types::Type::Fn(
                    vec![#(#param_types),*],
                    Box::new(#return_type)
                )
            };
        }
    }

    // Simple types
    match spec {
        "int" => quote! { crate::types::Type::Int },
        "str" => quote! { crate::types::Type::Str },
        "bool" => quote! { crate::types::Type::Bool },
        "proc" => quote! { crate::types::Type::Proc },
        "any" => quote! { crate::types::Type::Any },
        _ => quote! { crate::types::Type::Any },
    }
}

#[proc_macro]
pub fn setup_builtins(_item: TokenStream) -> TokenStream {
    let pure_builtins = ALL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .iter()
        .map(|s| {
            let ident = syn::Ident::new(s, proc_macro2::Span::call_site());
            quote! {
                #ident(builtins);
            }
        })
        .reduce(|a, b| quote! { #a #b });

    let eval_builtins = ALL_EVAL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .iter()
        .map(|s| {
            let ident = syn::Ident::new(s, proc_macro2::Span::call_site());
            quote! {
                #ident(builtins);
            }
        })
        .reduce(|a, b| quote! { #a #b });

    let setup_prog = match (pure_builtins, eval_builtins) {
        (Some(pure), Some(eval)) => quote! { #pure #eval },
        (Some(pure), None) => pure,
        (None, Some(eval)) => eval,
        (None, None) => panic!("No builtins found"),
    };

    quote! {
        #[allow(clippy::mutable_key_type)]
        pub fn setup_builtins(builtins: &mut BuiltinIndex) {
            #setup_prog
        }
    }
    .into()
}
