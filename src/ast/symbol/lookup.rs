//! A trie-based lookup for nonempty scoped symbols.

/// A key identifying a symbol.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct LookupKey(usize);

impl LookupKey {
    pub fn index(self) -> usize {
        self.0
    }
}

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
    pub fn get<S, I>(&self, key: I) -> Option<LookupKey>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        Self::get_node(&self.roots, key).and_then(|n| n.index)
    }

    /// Retrieve all values which are direct descendants of the given key string.
    pub fn get_all<S, I>(&self, scope: I) -> impl Iterator<Item = (&str, LookupKey)>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let node = Self::get_node(&self.roots, scope);
        node.into_iter()
            .flat_map(|n| &n.children)
            .flat_map(|n| Some((n.fragment.as_str(), n.index?)))
    }

    /// Place the value in the lookup. The symbol must be non-empty. If the symbol is
    /// already present, nothing is done.
    pub fn insert<S, I>(&mut self, key: I)
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
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
        let mut key_iter = key.into_iter();
        let mut current = find_or_create(&mut self.roots, key_iter.next().unwrap().as_ref());
        for fragment in key_iter {
            current = find_or_create(&mut current.children, fragment.as_ref());
        }
        if current.index.is_none() {
            let index = self.next_key;
            self.next_key += 1;
            current.index = Some(LookupKey(index));
        }
    }

    /// Resolve a key fragment against a possible longest prefix.
    pub fn resolve<S, T, I, J>(&self, scope: I, fragment: J) -> Option<LookupKey>
    where
        S: AsRef<str>,
        T: AsRef<str>,
        I: IntoIterator<Item = S>,
        J: IntoIterator<Item = T> + Clone,
    {
        let mut current_space = self.roots.as_slice();
        let mut nodes = Vec::new();
        for frag in scope {
            let node = current_space
                .iter()
                .find(|node| node.fragment.as_str() == frag.as_ref());
            if let Some(node) = node {
                nodes.push(node);
                current_space = node.children.as_slice();
            } else {
                current_space = &[];
            }
        }
        nodes
            .into_iter()
            .rev()
            .flat_map(|node| Self::get_node(&node.children, fragment.clone()))
            .flat_map(|node| node.index)
            .next()
    }

    /// Retrieve the node associated with a given key string from the given roots.
    fn get_node<S, I>(roots: &[Node], key: I) -> Option<&Node>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let mut key_iter = key.into_iter();
        let fragment = key_iter.next()?;
        let mut current = roots
            .iter()
            .find(|node| node.fragment.as_str() == fragment.as_ref())?;
        for fragment in key_iter {
            // current.fragment == key[i]
            current = current
                .children
                .iter()
                .find(|node| node.fragment.as_str() == fragment.as_ref())?;
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
        let mut none = lookup.get_all(&["a"]);
        assert_eq!(None, none.next());
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
        let key = lookup.resolve(&["a", "b", "c"], &["d"]).expect("d");
        assert_eq!(LookupKey(0), key);
        let key = lookup.resolve(&["a", "b", "c"], &["z"]).expect("z");
        assert_eq!(LookupKey(2), key);
    }

    #[test]
    fn resolve_multi() {
        let mut lookup = Lookup::new();
        let key1 = ["a", "b", "c", "d", "e"];
        let key2 = ["a", "b", "d", "e"];
        let key3 = ["a", "b", "d", "z"];
        let key4 = ["a", "d", "e"];
        let key5 = ["a", "d", "z"];
        lookup.insert(&key1);
        lookup.insert(&key2);
        lookup.insert(&key3);
        lookup.insert(&key4);
        lookup.insert(&key5);
        let key = lookup.resolve(&["a", "b", "c"], &["d", "e"]).expect("d");
        assert_eq!(LookupKey(0), key);
        let key = lookup.resolve(&["a", "b", "c"], &["d", "z"]).expect("z");
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
