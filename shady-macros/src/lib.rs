extern crate lazy_static;
extern crate proc_macro;
extern crate quote;

use lazy_static::lazy_static;

use crate::quote::ToTokens;
use proc_macro::*;
use quote::quote;
use std::sync::Mutex;
use syn::{parse_macro_input, ItemFn};

lazy_static! {
    static ref ALL_BUILTINS: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

#[proc_macro_attribute]
pub fn builtin(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let sig = input.sig;
    let ident = sig.ident;
    let override_fn_name = args.to_string();
    let fn_name = if override_fn_name.len() > 0 {
        override_fn_name
    } else {
        ident.to_string()
    };
    let fname = format!("setup_{}_builtin", ident);
    let setup_ident = syn::Ident::new(&fname, ident.span());
    let params = sig.inputs;
    let mut params_prog = quote! {};
    let mut args_prog = quote! {};
    let mut call_prog = quote! {};
    for (i, param) in params.clone().into_iter().enumerate() {
        match param {
            syn::FnArg::Typed(typed) => {
                let ident = match typed.pat.as_ref() {
                    syn::Pat::Ident(ident) => ident.ident.clone(),
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
    let block = input.block.to_token_stream();
    ALL_BUILTINS.lock().unwrap().push(fname);
    quote! {
        pub fn #setup_ident(builtins: &mut crate::eval::BuiltinIndex) {
            let fun = |#params| #block;
            let signature = crate::ast::FnSignature {
                fn_name: #fn_name.to_string(),
                parameters: vec![
                    #params_prog
                ],
                is_public: true,
                is_infix: false,
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
        .unwrap()
        .iter()
        .map(|s| {
            let ident = syn::Ident::new(s, proc_macro2::Span::call_site());
            quote! {
                #ident(builtins);
            }
        })
        .reduce(|a, b| quote! { #a #b })
        .unwrap();
    quote! {
        pub fn setup_builtins(builtins: &mut BuiltinIndex) {
            #setup_prog
        }
    }
    .into()
}
