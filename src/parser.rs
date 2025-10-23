use tree_sitter::Language;

extern "C" {
    fn tree_sitter_shady() -> Language;
}

/// Get the tree-sitter Language for Shady
pub fn language() -> Language {
    unsafe { tree_sitter_shady() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language() {
        let language = language();
        assert!(language.node_kind_count() > 0);
    }
}
