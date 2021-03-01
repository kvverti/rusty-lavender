//! A trie-based lookup for scoped symbols.

use crate::ast::symbol::AstSymbol;
use crate::parser::item::Fixity;
use crate::parser::tagged::Tagged;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct LookupKey(usize);

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct Lookup {
    values: Vec<(Tagged<AstSymbol>, Fixity)>,
    roots: Vec<Node>,
}

impl Lookup {
    /// Create an empty lookup.
    pub const fn new() -> Self {
        Self {
            values: Vec::new(),
            roots: Vec::new(),
        }
    }

    pub fn get_by_key(&self, key: LookupKey) -> &(Tagged<AstSymbol>, Fixity) {
        &self.values[key.0]
    }

    /// Retrieve the value associated with the given key string.
    pub fn get(&self, key: &[&str]) -> Option<LookupKey> {
        self.get_node(key).and_then(|n| n.index)
    }

    /// Retrieve all values which are direct descendants of the given key string.
    pub fn get_all(&self, scope: &[&str]) -> impl Iterator<Item = (&str, LookupKey)> {
        let node = self.get_node(scope);
        node.into_iter()
            .flat_map(|n| &n.children)
            .flat_map(|n| Some((n.fragment.as_str(), n.index?)))
    }

    /// Place the value in the lookup. The symbol must be non-empty.
    pub fn put_if_absent(&mut self, value: (Tagged<AstSymbol>, Fixity)) {
        // todo: why borrowck, why
        fn find_or_create<'a>(children: &'a mut Vec<Node>, fragment: &str) -> &'a mut Node {
            let node = children
                .iter_mut()
                .find(|node| node.fragment.as_str() == fragment);
            if let Some(node) = node {
                return node;
            }
            let node = Node {
                fragment: fragment.to_owned(),
                index: None,
                children: Vec::new(),
            };
            children.push(node);
            children.last_mut().unwrap()
        }
        let key = value.0.value.as_scopes();
        let mut current = find_or_create(&mut self.roots, &key[0]);
        for fragment in key[1..].iter() {
            current = find_or_create(&mut current.children, fragment.as_str());
        }
        let index = self.values.len();
        self.values.push(value);
        current.index = Some(LookupKey(index));
    }

    /// Resolve a key fragment against a possible longest prefix.
    pub fn resolve(&self, scope: &[&str], fragment: &str) -> Option<LookupKey> {
        for n in 0..scope.len() {
            let node = self.get_node(&scope[..n]);
            if let Some(node) = node {
                let key = node
                    .children
                    .iter()
                    .find(|node| node.fragment.as_str() == fragment)
                    .and_then(|node| node.index);
                if key.is_some() {
                    return key;
                }
            }
        }
        None
    }

    fn get_node(&self, key: &[&str]) -> Option<&Node> {
        let fragment = key.get(0)?;
        let mut current = self
            .roots
            .iter()
            .find(|node| node.fragment.as_str() == *fragment)?;
        for fragment in key[1..].iter() {
            // current.fragment == key[i]
            current = current
                .children
                .iter()
                .find(|node| node.fragment.as_str() == *fragment)?;
        }
        Some(current)
    }
}

/// A node in the lookup.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Node {
    fragment: String,
    index: Option<LookupKey>,
    children: Vec<Node>,
}
