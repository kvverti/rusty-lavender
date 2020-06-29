use crate::value::LvValue;

pub fn extract_int(v: &LvValue) -> i64 {
    use LvValue::Integer;
    if let Integer(v) = *v {
        v
    } else {
        panic!("Expected integer");
    }
}