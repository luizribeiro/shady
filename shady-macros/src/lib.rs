extern crate lazy_static;
extern crate proc_macro;
extern crate quote;

use lazy_static::lazy_static;

use proc_macro::*;
use quote::quote;
use std::sync::Mutex;
use syn::{parse_macro_input, AttributeArgs, ItemFn};

lazy_static! {
    static ref ALL_BUILTINS: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

#[proc_macro_attribute]
pub fn builtin(args: TokenStream, input: TokenStream) -> TokenStream {
    let builtin_fun = parse_macro_input!(input as ItemFn);
    let sig = &builtin_fun.sig;
    let builtin_fn_ident = &sig.ident;
    let mut fn_name = builtin_fn_ident.to_string();
    // TODO: implement support for varargs
    let mut _is_varargs = false;
    let mut is_infix = false;

    let args = parse_macro_input!(args as AttributeArgs);
    for arg in args {
        match arg {
            syn::NestedMeta::Meta(m) => match m {
                syn::Meta::NameValue(nv) => {
                    let name = nv.path.get_ident().unwrap().to_string();
                    let value = match nv.lit {
                        syn::Lit::Bool(b) => b.value(),
                        _ => panic!("Unsupported literal type"),
                    };
                    match name.as_str() {
                        "vargs" => _is_varargs = value,
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

    let setup_fname = format!("setup_{}_builtin", builtin_fn_ident);
    let setup_ident = syn::Ident::new(&setup_fname, builtin_fn_ident.span());
    let mut params_prog = quote! {};
    let mut args_prog = quote! {};
    let mut call_prog = quote! {};
    for (i, param) in sig.inputs.clone().into_iter().enumerate() {
        match param {
            syn::FnArg::Typed(typed) => {
                let ident = match typed.pat.as_ref() {
                    syn::Pat::Ident(builtin_fn_ident) => builtin_fn_ident.ident.clone(),
                    _ => panic!("Invalid parameter"),
                };
                let param_name = ident.to_string();
                let ty = &typed.ty;
                params_prog.extend(quote! {
                    crate::ast::Parameter {
                        name: #param_name.to_string(),
                        typ: crate::types::value_type::<#ty>(),
                    },
                });
                args_prog.extend(quote! {
                    let #ident = crate::types::from_value::<#ty>(args[#i].clone());
                });
                call_prog.extend(quote! { #ident, });
            }
            _ => panic!("Invalid parameter"),
        }
    }
    ALL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .push(setup_fname);
    quote! {
        #builtin_fun

        pub fn #setup_ident(builtins: &mut crate::eval::BuiltinIndex) {
            let fun = #builtin_fn_ident;
            let signature = crate::ast::FnSignature {
                fn_name: #fn_name.to_string(),
                parameters: vec![
                    #params_prog
                ],
                is_public: true,
                is_infix: #is_infix,
            };
            builtins.insert(
                signature,
                Box::new(move |args| {
                    #args_prog
                    let r = fun(#call_prog);
                    crate::types::to_value(r)
                }),
            );
        }
    }
    .into()
}

#[proc_macro]
pub fn setup_builtins(_item: TokenStream) -> TokenStream {
    let setup_prog = ALL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .iter()
        .map(|s| {
            let ident = syn::Ident::new(s, proc_macro2::Span::call_site());
            quote! {
                #ident(builtins);
            }
        })
        .reduce(|a, b| quote! { #a #b })
        .expect("failed to generate setup_builtins");
    quote! {
        pub fn setup_builtins(builtins: &mut BuiltinIndex) {
            #setup_prog
        }
    }
    .into()
}
