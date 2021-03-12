#[derive(Debug)]
pub enum Value {
    Nothing,
    Num(f64),
    Str(String),
    Atom(String),
}
