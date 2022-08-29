#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Ident(String),
    Num(i64),
    Bool(bool),
    Array(Vec<Box<Expr>>),
    Map(Vec<(Box<Expr>, Box<Expr>)>),
    TermRef((Box<Expr>, Box<Expr>)),
    Op(Box<Expr>, Opcode, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Plus,
}
