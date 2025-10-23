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
    let param_specs: Vec<&str> = param_spec.split(',').map(|s| s.trim()).collect();

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

    quote! {
        #orig_fun

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
                crate::eval::Builtin::Eval(Box::new(#orig_fun_ident)),
            );
        }
    }
    .into()
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
