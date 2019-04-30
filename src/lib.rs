//! Implements attribute macros for `lsp_msg` crate.
//!
//! These macros remove much of the repetition from the LSP definitions.
extern crate proc_macro;

use proc_macro::{TokenStream, TokenTree};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, Fields, Ident, Item, ItemStruct, Lit, Meta, MetaList, NestedMeta, Type,
};

/// An attribute-like macro to define LSP objects from structs.
///
/// # Examples
/// Base example that specifies struct is an LSP object.
/// ```
/// use lsp_msg_derive::lsp_object;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object]
/// struct LspObj {
/// }
/// ```
///
/// Allows deserializer to set missing fields to their respective default value.
/// ```
/// use lsp_msg_derive::lsp_object;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(allow_missing)]
/// struct LspObj {
/// }
/// ```
///
/// Adds `dynamic_registration` field to LSP object.
///
/// Literal value is added to the documentation string "Supports dynamic registration of the {}.".
/// ```
/// use lsp_msg_derive::lsp_object;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(dynamic_registration = "test")]
/// struct LspObj {
/// }
///
/// let lsp_obj = LspObj {
///     dynamic_registration: bool::default(),
/// };
/// ```
///
/// Adds `link_support` field to LSP object.
///
/// Literal value is added to the documentation string "Supports additional metadata in the form of
/// {} links.".
/// ```
/// use lsp_msg_derive::lsp_object;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(link_support = "test")]
/// struct LspObj {
/// }
///
/// let lsp_obj = LspObj {
///     link_support: bool::default(),
/// };
/// ```
///
/// Adds `trigger_characters` field to LSP object.
///
/// Literal value is added to the documentation string "Characters that trigger {} automatically.".
/// ```
/// use lsp_msg_derive::lsp_object;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(trigger_characters = "test")]
/// struct LspObj {
/// }
///
/// let lsp_obj = LspObj {
///     trigger_characters: vec![String::default()],
/// };
/// ```
///
/// Adds `resolve_provider` field to LSP object.
///
/// Literal value is added to the documentation string "Provides support to resolve additional information for a {} item.".
/// ```
/// use lsp_msg_derive::lsp_object;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(resolve_provider = "test")]
/// struct LspObj {
/// }
///
/// let lsp_obj = LspObj {
///     resolve_provider: bool::default(),
/// };
/// ```
///
/// Adds id field to LSP object.
/// ```
/// use lsp_msg_derive::lsp_object;
/// use lsp_msg_internal::Elective;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(static_registration)]
/// struct LspObj {
/// }
///
/// let lsp_obj = LspObj {
///     id: Elective::Present(String::new()),
/// };
/// ```
///
/// Adds a field to hold a `Vec<MarkupKind>`.
///
/// Literal value is added to the field name "{}_format" and the documentation string "The
/// supported `MarkupKind`s for the `{}` property.\n\nThe order describes the preferred format.".
/// ```
/// use lsp_msg_derive::lsp_object;
/// use lsp_msg_internal::MarkupKind;
/// use serde::{Deserialize, Serialize};
///
/// #[lsp_object(markup_kind_list = "test")]
/// struct LspObj {
/// }
///
/// let lsp_obj = LspObj {
///     test_format: Vec::new(),
/// };
/// ```
#[proc_macro_attribute]
#[inline]
pub fn lsp_object(metas: TokenStream, item: TokenStream) -> TokenStream {
    // Convert `metas` to `TokenStream2` so that it can be used within `quote`.
    let metas2 = TokenStream2::from(metas);
    let meta_list = TokenStream::from(quote! {lsp_object(#metas2)});
    let parsed_meta_list = parse_macro_input!(meta_list as MetaList);
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

    let mut fields: Vec<TokenStream2> = Vec::new();

    for metaitem in parsed_meta_list.nested {
        if let NestedMeta::Meta(meta) = metaitem {
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
                            let doc = format!(
                                "Supports dynamic registration of the {}.",
                                literal.value()
                            );
                            dynamic_registration_field = quote! {
                                #[doc = #doc]
                                dynamic_registration: bool,
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
                            let name = Ident::new(
                                &format!("{}_format", literal.value()),
                                Span::call_site(),
                            );
                            markup_kind_list_field = quote! {
                                #[doc = #doc]
                                #name: Vec<MarkupKind>,
                            };
                        }
                    }
                    "trigger_characters" => {
                        if let Lit::Str(literal) = name_value.lit {
                            let doc = format!(
                                "Characters that trigger {} automatically.",
                                literal.value()
                            );
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
                    _ => {}
                },
                Meta::List(_) => {
                    panic!("Meta List is not supported.");
                }
            }
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
        #[derive(Debug, Default, Deserialize, Serialize)]
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
            #(#fields),*
        }
    };

    TokenStream::from(output)
}

/// An attribute-like macro to define LSP kinds from enums.
#[proc_macro_attribute]
#[inline]
pub fn lsp_kind(metaitem: TokenStream, item: TokenStream) -> TokenStream {
    let mut kind_attrs = quote! {
        #[derive(Debug, Deserialize, Serialize)]
        #[serde(rename_all = "camelCase")]
    };

    for token in metaitem {
        if let TokenTree::Ident(ident) = token {
            if ident.to_string().as_str() == "number" {
                kind_attrs = quote! {
                    #[derive(Debug, Deserialize_repr, Serialize_repr)]
                    #[repr(u8)]
                };
            }
        }
    }

    let input = if let Item::Enum(item_enum) = parse_macro_input!(item as Item) {
        item_enum
    } else {
        panic!("Error");
    };

    let output = quote! {
        #kind_attrs
        #input
    };

    TokenStream::from(output)
}
