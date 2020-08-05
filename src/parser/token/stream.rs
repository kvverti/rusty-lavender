use nom::{Compare, CompareResult, InputTake, FindSubstring, Offset};

use crate::parser::token::{TokenStream, TokenValue};

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
        self.0.get(0)
            .map(|v| if v.value == t { CompareResult::Ok } else { CompareResult::Error })
            .unwrap_or(CompareResult::Incomplete)
    }

    fn compare_no_case(&self, t: TokenValue) -> CompareResult {
        self.compare(t)
    }
}

impl<'a> Offset for TokenStream<'a> {
    fn offset(&self, second: &Self) -> usize {
        second.0.as_ptr() as usize - self.0.as_ptr() as usize
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
