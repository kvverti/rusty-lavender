//! A trie-based lookup for nonempty scoped symbols.

/// An opaque key identifying a symbol.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct LookupKey(usize);

/// A node in the lookup.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Node {
    fragment: String,
    index: Option<LookupKey>,
    children: Vec<Node>,
}

/// A storage for nonempty hierarchical symbols.
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct Lookup {
    next_key: usize,
    roots: Vec<Node>,
}

impl Lookup {
    /// Create an empty lookup.
    pub const fn new() -> Self {
        Self {
            next_key: 0,
            roots: Vec::new(),
        }
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

    /// Place the value in the lookup. The symbol must be non-empty. If the symbol is
    /// already present, nothing is done.
    pub fn insert(&mut self, key: &[&str]) {
        fn find_or_create<'a>(children: &'a mut Vec<Node>, fragment: &str) -> &'a mut Node {
            let idx = children
                .iter_mut()
                .position(|node| node.fragment.as_str() == fragment)
                .unwrap_or_else(|| {
                    let node = Node {
                        fragment: fragment.to_owned(),
                        index: None,
                        children: Vec::new(),
                    };
                    let res = children.len();
                    children.push(node);
                    res
                });
            &mut children[idx]
        }
        let mut current = find_or_create(&mut self.roots, &key[0]);
        for fragment in key[1..].iter() {
            current = find_or_create(&mut current.children, fragment);
        }
        if current.index.is_none() {
            let index = self.next_key;
            self.next_key += 1;
            current.index = Some(LookupKey(index));
        }
    }

    /// Resolve a key fragment against a possible longest prefix.
    pub fn resolve(&self, scope: &[&str], fragment: &str) -> Option<LookupKey> {
        for n in (0..=scope.len()).rev() {
            let key = self
                .get_all(&scope[..n])
                .find(|&(f, _)| f == fragment)
                .map(|(_, k)| k);
            if key.is_some() {
                return key;
            }
        }
        None
    }

    /// Retrieve the node associated with a given key string.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut lookup = Lookup::new();
        let key1 = ["a", "b", "c"];
        let key2 = ["a", "b", "d"];
        let key3 = ["b", "b", "c"];

        // get and put
        assert!(lookup.get(&key1).is_none());
        assert!(lookup.get(&key2).is_none());
        assert!(lookup.get(&key3).is_none());
        lookup.insert(&key1);
        assert_eq!(LookupKey(0), lookup.get(&key1).expect("key1"));
        assert!(lookup.get(&key2).is_none());
        assert!(lookup.get(&key3).is_none());
        lookup.insert(&key2);
        lookup.insert(&key3);
        assert_eq!(LookupKey(0), lookup.get(&key1).expect("key1"));
        assert_eq!(LookupKey(1), lookup.get(&key2).expect("key2"));
        assert_eq!(LookupKey(2), lookup.get(&key3).expect("key3"));

        // get_all
        let all = lookup.get_all(&["a", "b"]).collect::<Vec<_>>();
        assert_eq!(&[("c", LookupKey(0)), ("d", LookupKey(1))], all.as_slice());
        let none = lookup.get_all(&["a"]).collect::<Vec<_>>();
        assert_eq!(0, none.len());
    }

    #[test]
    fn resolve() {
        let mut lookup = Lookup::new();
        let key1 = ["a", "b", "c", "d"];
        let key2 = ["a", "b", "d"];
        let key3 = ["a", "b", "z"];
        let key4 = ["a", "d"];
        let key5 = ["a", "z"];
        lookup.insert(&key1);
        lookup.insert(&key2);
        lookup.insert(&key3);
        lookup.insert(&key4);
        lookup.insert(&key5);
        let key = lookup.resolve(&["a", "b", "c"], "d").expect("d");
        assert_eq!(LookupKey(0), key);
        let key = lookup.resolve(&["a", "b", "c"], "z").expect("z");
        assert_eq!(LookupKey(2), key);
    }

    #[test]
    fn double_insert() {
        let mut lookup = Lookup::new();
        let key = ["a", "b", "c"];
        assert!(lookup.get(&key).is_none());
        lookup.insert(&key);
        assert_eq!(LookupKey(0), lookup.get(&key).expect("key"));
        lookup.insert(&key);
        assert_eq!(LookupKey(0), lookup.get(&key).expect("key"));
    }
}
