use nom::{
    Compare, CompareResult, FindSubstring, InputIter, InputLength, InputTake, Offset,
    UnspecializedInput,
};

use crate::parser::token::{Token, TokenStream, TokenValue};

impl<'a> InputTake for TokenStream<'a> {
    fn take(&self, count: usize) -> Self {
        TokenStream(&self.0[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let before = &self.0[..count];
        let after = &self.0[count..];
        (TokenStream(after), TokenStream(before))
    }
}

impl<'a> Compare<TokenValue> for TokenStream<'a> {
    fn compare(&self, t: TokenValue) -> CompareResult {
        self.0
            .get(0)
            .map(|v| {
                if v.value == t {
                    CompareResult::Ok
                } else {
                    CompareResult::Error
                }
            })
            .unwrap_or(CompareResult::Incomplete)
    }

    fn compare_no_case(&self, t: TokenValue) -> CompareResult {
        self.compare(t)
    }
}

impl<'a> Offset for TokenStream<'a> {
    fn offset(&self, second: &Self) -> usize {
        (second.0.as_ptr() as usize - self.0.as_ptr() as usize) / std::mem::size_of::<Token>()
    }
}

impl<'a> FindSubstring<TokenValue> for TokenStream<'a> {
    fn find_substring(&self, substr: TokenValue) -> Option<usize> {
        for (idx, token) in self.0.iter().enumerate() {
            if token.value == substr {
                return Some(idx);
            }
        }
        None
    }
}

impl<'a> InputLength for TokenStream<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> InputIter for TokenStream<'a> {
    type Item = &'a TokenValue;
    type Iter = IndexIter<'a>;
    type IterElem = Iter<'a>;

    fn iter_indices(&self) -> Self::Iter {
        IndexIter {
            tokens: self.0,
            idx: 0,
        }
    }

    fn iter_elements(&self) -> Self::IterElem {
        Iter { tokens: self.0 }
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        for (idx, Token { value, .. }) in self.0.iter().enumerate() {
            if predicate(value) {
                return Some(idx);
            }
        }
        None
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        if count < self.input_len() {
            Some(count)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Iter<'a> {
    tokens: &'a [Token],
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a TokenValue;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.tokens.is_empty() {
            let token = &self.tokens[0];
            self.tokens = &self.tokens[1..];
            Some(&token.value)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct IndexIter<'a> {
    tokens: &'a [Token],
    idx: usize,
}

impl<'a> Iterator for IndexIter<'a> {
    type Item = (usize, &'a TokenValue);

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.tokens.len() {
            let i = self.idx;
            self.idx += 1;
            Some((i, &self.tokens[i].value))
        } else {
            None
        }
    }
}

impl<'a> UnspecializedInput for TokenStream<'a> {}
