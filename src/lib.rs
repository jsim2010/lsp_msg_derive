//! Implements attribute macros for `lsp_msg` crate.
//!
//! These macros remove much of the repetition from the LSP definitions.
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use std::ops::Index;
use syn::{
    parse_macro_input, AttributeArgs, Fields, Ident, Item, ItemStruct, Lit, Meta, NestedMeta, Type,
};

/// Generates **LSP objects** from structs.
#[proc_macro_attribute]
#[inline]
pub fn lsp_object(args: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let item = parse_macro_input!(item as ItemStruct);

    let name = item.ident;
    let vis = item.vis;
    let generics = item.generics;
    let attrs = item.attrs;
    let old_fields = if let Fields::Named(fields_named) = item.fields {
        fields_named.named
    } else {
        panic!("Error");
    };
    let mut allow_missing_fields_attr = quote! {};
    let mut registration_id_field = quote! {};
    let mut dynamic_registration_field = quote! {};
    let mut link_support_field = quote! {};
    let mut markup_kind_list_field = quote! {};
    let mut trigger_characters_field = quote! {};
    let mut resolve_provider_field = quote! {};
    let mut value_set_field = quote! {};

    let mut fields: Vec<TokenStream2> = Vec::new();

    for meta in args.into_iter().filter_map(|arg| {
        if let NestedMeta::Meta(meta) = arg {
            Some(meta)
        } else {
            None
        }
    }) {
        match meta {
            Meta::Word(word) => match word.to_string().as_str() {
                "allow_missing" => {
                    allow_missing_fields_attr = quote! {
                        #[serde(default)]
                    };
                }
                "static_registration" => {
                    registration_id_field = quote! {
                        #[serde(default, skip_serializing_if = "Elective::is_absent")]
                        /// The id used to register the request.
                        id: Elective<String>,
                    };
                }
                _ => {}
            },
            Meta::NameValue(name_value) => match name_value.ident.to_string().as_str() {
                "dynamic_registration" => {
                    if let Lit::Str(literal) = name_value.lit {
                        let doc =
                            format!("Supports dynamic registration of the {}.", literal.value());
                        dynamic_registration_field = quote! {
                            #[doc = #doc]
                            #vis dynamic_registration: bool,
                        };
                    }
                }
                "link_support" => {
                    if let Lit::Str(literal) = name_value.lit {
                        let doc = format!(
                            "Supports additional metadata in the form of {} links.",
                            literal.value()
                        );
                        link_support_field = quote! {
                            #[doc = #doc]
                            link_support: bool,
                        };
                    }
                }
                "markup_kind_list" => {
                    if let Lit::Str(literal) = name_value.lit {
                        let doc = format!("The supported `MarkupKind`s for the `{}` property.\n\nThe order describes the preferred format.", literal.value());
                        let name =
                            Ident::new(&format!("{}_format", literal.value()), Span::call_site());
                        markup_kind_list_field = quote! {
                            #[doc = #doc]
                            #name: Vec<MarkupKind>,
                        };
                    }
                }
                "trigger_characters" => {
                    if let Lit::Str(literal) = name_value.lit {
                        let doc =
                            format!("Characters that trigger {} automatically.", literal.value());
                        trigger_characters_field = quote! {
                            #[doc = #doc]
                            trigger_characters: Vec<String>,
                        };
                    }
                }
                "resolve_provider" => {
                    if let Lit::Str(literal) = name_value.lit {
                        let doc = format!(
                            "Provides support to resolve additional information for a {} item.",
                            literal.value()
                        );
                        resolve_provider_field = quote! {
                            #[doc = #doc]
                            resolve_provider: bool,
                        };
                    }
                }
                value => {
                    panic!("Meta NameValue '{}' is not supported.", value);
                }
            },
            Meta::List(list) => match list.ident.to_string().as_str() {
                "value_set" => {
                    let len = list.nested.len();
                    let mut valid_doc = String::new();

                    if len == 2 {
                        if let NestedMeta::Literal(Lit::Str(valid)) = &list.nested.index(1) {
                            valid_doc =
                                format!(" Else, only supports values where `{}`.", valid.value());
                        }
                    }

                    if len == 1 || len == 2 {
                        if let NestedMeta::Meta(Meta::Word(kind_ident)) = &list.nested.index(0) {
                            let doc = format!("The supported `{0}` values.\n\nIf `Elective::is_absent()` is false, unknown values fall back to `{0}::default()`.{1}", kind_ident, valid_doc);
                            value_set_field = quote! {
                                #[doc = #doc]
                                #vis value_set: Elective<Vec<#kind_ident>>,
                            };
                        }
                    }
                }
                value => {
                    panic!("Meta List '{}' is not supported.", value);
                }
            },
        }
    }

    for field in old_fields {
        let mut elective_attr = quote! {};
        let ty = field.ty;

        if let Type::Path(ref p) = ty {
            if let Some(segment) = p.path.segments.first() {
                if segment.value().ident == "Elective" {
                    elective_attr = quote! {
                        #[serde(default, skip_serializing_if = "Elective::is_absent")]
                    };
                }
            }
        }
        let name = field.ident;
        let attrs = field.attrs;
        let vis = field.vis;
        fields.push(quote! {
            #elective_attr
            #(#attrs)*
            #vis #name: #ty
        });
    }

    let output = quote! {
        #[derive(Debug, Default, Deserialize, PartialEq, Serialize)]
        #[serde(rename_all = "camelCase")]
        #allow_missing_fields_attr
        #(#attrs)*
        #vis struct #name #generics {
            #registration_id_field
            #dynamic_registration_field
            #link_support_field
            #markup_kind_list_field
            #trigger_characters_field
            #resolve_provider_field
            #value_set_field
            #(#fields),*
        }
    };

    TokenStream::from(output)
}

/// Defines LSP kinds from enums.
#[proc_macro_attribute]
#[inline]
pub fn lsp_kind(args: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let default_derives = quote! {
        Debug, PartialEq
    };
    let mut derives = quote! {
        Deserialize, Serialize
    };
    let mut serde_attrs = quote! {
        #[serde(untagged)]
    };

    for arg in args {
        if let NestedMeta::Meta(meta) = arg {
            if let Meta::NameValue(name_value) = meta {
                if name_value.ident.to_string().as_str() == "type" {
                    if let Lit::Str(literal) = name_value.lit {
                        match literal.value().as_str() {
                            "string" => {
                                serde_attrs = quote! {
                                    #[serde(rename_all = "camelCase")]
                                };
                            }
                            "number" => {
                                derives = quote! {
                                    Deserialize_repr, Serialize_repr
                                };
                                serde_attrs = quote! {
                                    #[repr(u8)]
                                };
                            }
                            "language_id" => {
                                serde_attrs = quote! {
                                    #[serde(rename_all = "kebab-case")]
                                };
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    let input = if let Item::Enum(item_enum) = parse_macro_input!(item as Item) {
        item_enum
    } else {
        panic!("Error");
    };

    let output = quote! {
        #[derive(#default_derives, #derives)]
        #serde_attrs
        #input
    };

    TokenStream::from(output)
}
