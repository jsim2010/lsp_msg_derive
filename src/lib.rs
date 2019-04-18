//! Implements macros for `lsp_msg` crate.
//!
//! These macros remove much of the repetition from the LSP definitions.
extern crate proc_macro;

use proc_macro::{Literal, TokenStream, TokenTree};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_macro_input, Fields, Ident, Item, Type};

/// Holds the state of the attribute parser.
enum AttributeParserState {
    /// Ready to parse next attribute.
    Option,
    /// Ready to parse value assigned to dynamic_registration option.
    DynamicRegistrationValue,
    /// Ready to parse value assigned to link_support option.
    LinkSupportValue,
    /// Ready to parse value assigned to markup_kind_list option.
    MarkupKindListValue,
    /// Ready to parse value assigned to triggers option.
    TriggersValue,
    /// Ready to parse value assigned to resolve_provider option.
    ResolveProviderValue,
}

impl AttributeParserState {
    /// If parser is currently at a valid end.
    fn is_complete(&self) -> bool {
        match self {
            AttributeParserState::Option => false,
            _ => true,
        }
    }
}

/// Defines attributes of the `lsp_object` attribute.
#[derive(Default)]
struct Attributes {
    /// Sets struct when deserializing to set any missing field to its default.
    pub(crate) allow_missing_attr: TokenStream2,
    /// Includes dynamic registration parameter with given documentation variable.
    pub(crate) dynamic_registration: Option<Literal>,
    /// Includes link support parameter with given documentation variable.
    pub(crate) link_support: Option<Literal>,
    /// Includes markup kind parameter with given documentation variable.
    pub(crate) markup_kind_list: Option<Literal>,
    /// Includes resolve provider parameter with given documentation variable.
    pub(crate) triggers: Option<Literal>,
    /// Includes resolve provider parameter with given documentation variable.
    pub(crate) resolve_provider: Option<Literal>,
    /// Includes text document registration parameter(s).
    pub(crate) has_document_selector: bool,
    /// Includes static registration parameter(s).
    pub(crate) has_static_registration: bool,
}

/// Retrieves the `Attributes` provided to `lsp_object` attribute.
fn get_attributes(attr: TokenStream) -> Attributes {
    let mut state = AttributeParserState::Option;
    let mut attributes = Attributes::default();

    for token in attr {
        match state {
            AttributeParserState::Option => {
                if let TokenTree::Ident(ident) = token {
                    match ident.to_string().as_str() {
                        "allow_missing" => {
                            attributes.allow_missing_attr = quote! {
                                #[serde(default)]
                            };
                        }
                        "document_selector" => {
                            attributes.has_document_selector = true;
                        }
                        "static_registration" => {
                            attributes.has_static_registration = true;
                        }
                        "dynamic_registration" => {
                            state = AttributeParserState::DynamicRegistrationValue;
                        }
                        "link_support" => {
                            state = AttributeParserState::LinkSupportValue;
                        }
                        "markup_kind_list" => {
                            state = AttributeParserState::MarkupKindListValue;
                        }
                        "triggers" => {
                            state = AttributeParserState::TriggersValue;
                        }
                        "resolve_provider" => {
                            state = AttributeParserState::ResolveProviderValue;
                        }
                        option => {
                            panic!("Unsupported attribute option: {}", option);
                        }
                    }
                }
            }
            AttributeParserState::DynamicRegistrationValue => {
                if let TokenTree::Literal(literal) = token {
                    attributes.dynamic_registration = Some(literal);
                    state = AttributeParserState::Option;
                }
            }
            AttributeParserState::LinkSupportValue => {
                if let TokenTree::Literal(literal) = token {
                    attributes.link_support = Some(literal);
                    state = AttributeParserState::Option;
                }
            }
            AttributeParserState::MarkupKindListValue => {
                if let TokenTree::Literal(literal) = token {
                    attributes.markup_kind_list = Some(literal);
                    state = AttributeParserState::Option;
                }
            }
            AttributeParserState::TriggersValue => {
                if let TokenTree::Literal(literal) = token {
                    attributes.triggers = Some(literal);
                    state = AttributeParserState::Option;
                }
            }
            AttributeParserState::ResolveProviderValue => {
                if let TokenTree::Literal(literal) = token {
                    attributes.resolve_provider = Some(literal);
                    state = AttributeParserState::Option;
                }
            }
        }
    }

    if state.is_complete() {
        panic!("Missing a value for an option.");
    }

    attributes
}

/// An attribute-like macro to define LSP objects from structs.
#[proc_macro_attribute]
#[inline]
pub fn lsp_object(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attributes = get_attributes(attr);
    let input = if let Item::Struct(item_struct) = parse_macro_input!(item as Item) {
        item_struct
    } else {
        panic!("Error");
    };

    let name = input.ident;
    let vis = input.vis;
    let generics = input.generics;
    let attrs = input.attrs;
    let old_fields = if let Fields::Named(fields_named) = input.fields {
        fields_named.named
    } else {
        panic!("Error");
    };

    let mut fields: Vec<TokenStream2> = Vec::new();

    if attributes.has_document_selector {
        fields.push(quote! {
            /// Identifies the scope of the registration.
            ///
            /// If `Option::None`, `DocumentSelector` provided by client will be used.
            document_selector: Option<char>
        });
    }

    if attributes.has_static_registration {
        fields.push(quote! {
            /// The id used to register the request.
            id: Elective<String>
        });
    }

    if let Some(doc_var) = attributes.dynamic_registration {
        let mut d = doc_var.to_string();
        d.retain(|c| c != '"');
        let doc = format!("Supports dynamic registration of the {}.", d);

        fields.push(quote! {
            #[doc = #doc]
            dynamic_registration: bool
        });
    }

    if let Some(doc_var) = attributes.link_support {
        let mut d = doc_var.to_string();
        d.retain(|c| c != '"');
        let doc = format!("Supports additional metadata in the form of {} links.", d);

        fields.push(quote! {
            #[doc = #doc]
            link_support: bool
        });
    }

    if let Some(property) = attributes.markup_kind_list {
        let mut p = property.to_string();
        p.retain(|c| c != '"');
        let doc = format!("The supported `MarkupKind`s for the `{}` property.\n\nThe order describes the preferred format.", p);
        let property_name = format!("{}_format", p);
        let name = Ident::new(&property_name, Span::call_site());

        fields.push(quote! {
            #[doc = #doc]
            #name: Vec<MarkupKind>
        });
    }

    if let Some(doc_var) = attributes.triggers {
        let mut d = doc_var.to_string();
        d.retain(|c| c != '"');
        let doc = format!("Characters that trigger {} automatically.", d);

        fields.push(quote! {
            #[doc = #doc]
            trigger_characters: Vec<String>
        });
    }

    if let Some(doc_var) = attributes.resolve_provider {
        let mut d = doc_var.to_string();
        d.retain(|c| c != '"');
        let doc = format!(
            "Provides support to resolve additional information for a {} item.",
            d
        );

        fields.push(quote! {
            #[doc = #doc]
            resolve_provider: bool
        });
    }

    for field in old_fields {
        let mut is_elective = false;

        if let Type::Path(ref p) = field.ty {
            if let Some(segment) = p.path.segments.first() {
                if segment.value().ident == "Elective" {
                    is_elective = true;
                }
            }
        }
        let elective_attr = if is_elective {
            quote! {
                #[serde(default, skip_serializing_if = "Elective::is_absent")]
            }
        } else {
            quote! {}
        };
        let field_type = field.ty;
        let field_name = field.ident;
        let field_attrs = field.attrs;
        let field_vis = field.vis;
        fields.push(quote! {
            #elective_attr
            #(#field_attrs)*
            #field_vis #field_name: #field_type
        });
    }

    let allow_missing_attr = attributes.allow_missing_attr;

    let output = quote! {
        #[derive(Debug, Default, Deserialize, Serialize)]
        #[serde(rename_all = "camelCase")]
        #allow_missing_attr
        #(#attrs)*
        #vis struct #name #generics {
            #(#fields),*
        }
    };

    TokenStream::from(output)
}

/// An attribute-like macro to define LSP kinds from enums.
#[proc_macro_attribute]
#[inline]
pub fn lsp_kind(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut kind_attrs = quote! {
        #[derive(Debug, Deserialize, Serialize)]
        #[serde(rename_all = "camelCase")]
    };

    for token in attr {
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
