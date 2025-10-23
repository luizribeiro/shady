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

/// Macro for Eval builtins (special forms) that need access to unevaluated expressions
/// Usage: #[eval_builtin("name", param_types, return_type)]
/// Where param_types is a string like "fn(any)->any, [any]" representing the signature
#[proc_macro_attribute]
pub fn eval_builtin(args: TokenStream, input: TokenStream) -> TokenStream {
    let orig_fun = parse_macro_input!(input as ItemFn);
    let macro_args = parse_macro_input!(args as AttributeArgs);

    let orig_fun_ident = &orig_fun.sig.ident;

    // Parse macro arguments: fn_name, param_spec, return_type
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

    quote! {
        #orig_fun

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

            // Call the actual implementation
            #orig_fun_ident(args, local_context, context)
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
