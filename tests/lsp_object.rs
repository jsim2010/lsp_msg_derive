use lsp_msg_derive::lsp_object;
use lsp_msg_internal::{Elective, MarkupKind};
use serde::{Deserialize, Serialize};
use serde_test::{assert_de_tokens, assert_tokens, Token};

#[test]
fn serde() {
    #[lsp_object]
    struct LspObj {}

    assert_tokens(
        &LspObj {},
        &[
            Token::Struct {
                name: "LspObj",
                len: 0,
            },
            Token::StructEnd,
        ],
    );
}

#[test]
fn allow_missing() {
    #[lsp_object(allow_missing)]
    struct LspObj {
        optional_field: bool,
    }
    let lsp_obj = LspObj {
        optional_field: bool::default(),
    };

    assert_de_tokens(
        &lsp_obj,
        &[
            Token::Struct {
                name: "LspObj",
                len: 0,
            },
            Token::StructEnd,
        ],
    );
}

#[test]
fn dynamic_registration() {
    #[lsp_object(dynamic_registration = "test")]
    struct LspObj {}

    LspObj {
        dynamic_registration: bool::default(),
    };
}

#[test]
fn link_support() {
    use lsp_msg_derive::lsp_object;
    use serde::{Deserialize, Serialize};

    #[lsp_object(link_support = "test")]
    struct LspObj {}

    LspObj {
        link_support: bool::default(),
    };
}

#[test]
fn trigger_characters() {
    #[lsp_object(trigger_characters = "test")]
    struct LspObj {}

    LspObj {
        trigger_characters: vec![String::default()],
    };
}

#[test]
fn resolve_provider() {
    #[lsp_object(resolve_provider = "test")]
    struct LspObj {}

    LspObj {
        resolve_provider: bool::default(),
    };
}

#[test]
fn static_registration() {
    #[lsp_object(static_registration)]
    struct LspObj {}

    LspObj {
        id: Elective::Present(String::default()),
    };
}

#[test]
fn markup_kind_list() {
    #[lsp_object(markup_kind_list = "test")]
    struct LspObj {}

    LspObj {
        test_format: vec![MarkupKind::Plaintext],
    };
}

#[test]
fn value_set_support_all() {
    #[lsp_object(value_set(bool))]
    struct LspObj {}

    LspObj {
        value_set: Elective::Present(vec![bool::default()]),
    };
}

#[test]
fn value_set_support_some() {
    #[lsp_object(value_set(bool, "test"))]
    struct LspObj {}

    LspObj {
        value_set: Elective::Present(vec![bool::default()]),
    };
}
