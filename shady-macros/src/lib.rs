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
}

struct Builtin {
    fn_name: String,
    is_infix: bool,
    // TODO: implement support for varargs
    #[allow(dead_code)]
    is_vargs: bool,
}

impl Builtin {
    fn new(orig_fun: &ItemFn, macro_args: &Vec<NestedMeta>) -> Self {
        let mut fn_name = orig_fun.sig.ident.to_string();
        let mut is_infix = false;
        let mut is_vargs = false;

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
                            "vargs" => is_vargs = value,
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
            is_vargs,
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

    let setup_fname = format!("setup_{}_builtin", orig_fun_ident);
    ALL_BUILTINS
        .lock()
        .expect("could not obtain lock")
        .push(setup_fname.clone());

    let builtin = Builtin::new(&orig_fun, &macro_args);
    let (fn_name, is_infix) = (builtin.fn_name, builtin.is_infix);
    let setup_ident = syn::Ident::new(&setup_fname, orig_fun_ident.span());

    quote! {
        #orig_fun

        pub fn #setup_ident(builtins: &mut crate::eval::BuiltinIndex) {
            let fun = #orig_fun_ident;
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
