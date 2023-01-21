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
    let block = input.block.to_token_stream();
    ALL_BUILTINS.lock().unwrap().push(fname);
    quote! {
        fn #setup_ident(builtins: &mut BuiltinIndex) {
            let fun = |#params| #block;
            let signature = FnSignature {
                fn_name: #fn_name.to_string(),
                parameters: vec![
                    Parameter {
                        name: "x".to_string(),
                        typ: i64::value_type(),
                    },
                    Parameter {
                        name: "x".to_string(),
                        typ: i64::value_type(),
                    },
                ],
                is_public: true,
                is_infix: false,
            };
            builtins.insert(
                signature,
                Box::new(move |args| {
                    let a = i64::from_value(args[0].clone());
                    let b = i64::from_value(args[1].clone());
                    let r = fun(a, b);
                    r.to_value()
                }),
            );
        }
    }
    .into()
}

#[proc_macro]
pub fn setup_builtins(_item: TokenStream) -> TokenStream {
    let x = ALL_BUILTINS
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
    x.into()
}
