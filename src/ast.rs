use crate::lexer::Span;
use crate::token::Token;

#[derive(Clone)]
pub enum Statement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    Expression(ExpressionStatement),
    Let(LetStatement),
    Return(ReturnStatement),
    Import(ImportStatement),
    Export(ExportStatement),
    Struct(StructStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    While(WhileStatement),
    Assign(AssignStatement),
    ForIn(ForInStatement),
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Block(v) => v.span,
            Statement::Empty(v) => v.span,
            Statement::Expression(v) => v.span,
            Statement::Let(v) => v.span,
            Statement::Return(v) => v.span,
            Statement::Import(v) => v.span,
            Statement::Export(v) => v.span,
            Statement::Struct(v) => v.span,
            Statement::Break(v) => v.span,
            Statement::Continue(v) => v.span,
            Statement::While(v) => v.span,
            Statement::Assign(v) => v.span,
            Statement::ForIn(v) => v.span,
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Bool(BoolLiteral),
    Null(NullLiteral),
    Float(FloatLiteral),
    Integer(IntegerLiteral),
    String(StringLiteral),
    Function(FunctionLiteral),
    Call(Box<CallExpression>),
    Identifier(Identifier),
    If(Box<IfExpression>),
    Infix(Box<InfixExpression>),
    Prefix(Box<PrefixExpression>),
    Slice(SliceLiteral),
    Map(MapLiteral),
    StructLiteral(StructLiteral),
    Index(Box<IndexExpression>),
    Member(Box<MemberExpression>),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Bool(v) => v.span,
            Expression::Null(v) => v.span,
            Expression::Float(v) => v.span,
            Expression::Integer(v) => v.span,
            Expression::String(v) => v.span,
            Expression::Function(v) => v.span,
            Expression::Call(v) => v.span,
            Expression::Identifier(v) => v.span,
            Expression::If(v) => v.span,
            Expression::Infix(v) => v.span,
            Expression::Prefix(v) => v.span,
            Expression::Slice(v) => v.span,
            Expression::Map(v) => v.span,
            Expression::StructLiteral(v) => v.span,
            Expression::Index(v) => v.span,
            Expression::Member(v) => v.span,
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
    pub path: Option<String>,
}

#[derive(Clone)]
pub struct LetStatement {
    pub span: Span,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub span: Span,
    pub value: Expression,
}

#[derive(Clone)]
pub struct ImportStatement {
    pub span: Span,
    pub alias: Option<Identifier>,
    pub path: StringLiteral,
}

#[derive(Clone)]
pub struct ExportStatement {
    pub span: Span,
    pub name: Identifier,
}

#[derive(Clone)]
pub struct StructStatement {
    pub span: Span,
    pub name: Identifier,
    pub fields: Vec<Identifier>,
}

#[derive(Clone)]
pub struct BreakStatement {
    pub span: Span,
}

#[derive(Clone)]
pub struct ContinueStatement {
    pub span: Span,
}

#[derive(Clone)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: Expression,
    /// if true, express statement value is empty value
    pub end_of_semicolon: bool,
}

#[derive(Clone)]
pub struct EmptyStatement {
    pub span: Span,
}

#[derive(Clone)]
pub struct BlockStatement {
    pub span: Span,
    pub statements: Vec<Statement>,
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub span: Span,
    pub token: Token,
    pub right: Expression,
}

#[derive(Clone)]
pub struct InfixExpression {
    pub span: Span,
    pub token: Token,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub span: Span,
    pub value: i64,
}

#[derive(Clone)]
pub struct FloatLiteral {
    pub span: Span,
    pub value: f64,
}

#[derive(Clone)]
pub struct BoolLiteral {
    pub span: Span,
    pub value: bool,
}

#[derive(Clone)]
pub struct NullLiteral {
    pub span: Span,
}

#[derive(Clone)]
pub struct StringLiteral {
    pub span: Span,
    pub value: String,
}

#[derive(Clone)]
pub struct IfExpression {
    pub span: Span,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
    pub optional: Option<Box<Expression>>,
}

#[derive(Clone)]
pub struct WhileStatement {
    pub span: Span,
    pub condition: Expression,
    pub consequence: BlockStatement,
}

#[derive(Clone)]
pub struct ForInStatement {
    pub span: Span,
    pub collection: Expression,
    pub key: Identifier,
    pub value: Option<Identifier>,
    pub body: BlockStatement,
}

#[derive(Clone)]
pub struct AssignStatement {
    pub span: Span,
    pub target: AssignTarget,
    pub value: Expression,
}

#[derive(Clone)]
pub enum AssignTarget {
    Identifier(Identifier),
    Index {
        left: Expression,
        index: Expression,
    },
    Member {
        left: Expression,
        property: Identifier,
    },
}

#[derive(Clone)]
pub struct FunctionLiteral {
    pub span: Span,
    pub name: Option<Identifier>,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

#[derive(Clone)]
pub struct CallExpression {
    pub span: Span,
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Clone)]
pub struct MapLiteral {
    pub span: Span,
    pub kv_pair: Vec<(Expression, Expression)>,
}

#[derive(Clone)]
pub struct StructLiteral {
    pub span: Span,
    pub name: Box<Expression>,
    pub fields: Vec<(Identifier, Expression)>,
    pub values: Vec<Expression>,
}

#[derive(Clone)]
pub struct SliceLiteral {
    pub span: Span,
    pub elements: Vec<Expression>,
}

#[derive(Clone)]
pub struct IndexExpression {
    pub span: Span,
    pub left: Expression,
    pub index: Expression,
}

#[derive(Clone)]
pub struct MemberExpression {
    pub span: Span,
    pub left: Expression,
    pub property: Identifier,
}
