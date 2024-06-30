use crate::lexer::Position;
use crate::token::Token;

#[derive(Clone)]
pub enum Statement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    Expression(ExpressionStatement),
    Let(LetStatement),
    Return(ReturnStatement),
    While(WhileStatement),
    IndexAssign(IndexAssignStatement),
    ForIn(ForInStatement),
}

impl Statement {
    pub fn string(&self) -> String {
        match self {
            Statement::Block(v) => v.string(),
            Statement::Empty(v) => v.string(),
            Statement::Expression(v) => v.string(),
            Statement::Let(v) => v.string(),
            Statement::Return(v) => v.string(),
            Statement::While(v) => v.string(),
            Statement::IndexAssign(v) => v.string(),
            Statement::ForIn(v) => v.string(),
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Statement::Block(v) => v.position.to_owned(),
            Statement::Empty(v) => v.position.to_owned(),
            Statement::Expression(v) => v.position.to_owned(),
            Statement::Let(v) => v.position.to_owned(),
            Statement::Return(v) => v.position.to_owned(),
            Statement::While(v) => v.position.to_owned(),
            Statement::IndexAssign(v) => v.position.to_owned(),
            Statement::ForIn(v) => v.position.to_owned(),
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Bool(BoolLiteral),
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
    Index(Box<IndexExpression>),
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::Bool(v) => v.string(),
            Expression::Float(v) => v.string(),
            Expression::Integer(v) => v.string(),
            Expression::String(v) => v.string(),
            Expression::Function(v) => v.string(),
            Expression::Call(v) => v.string(),
            Expression::Identifier(v) => v.string(),
            Expression::If(v) => v.string(),
            Expression::Infix(v) => v.string(),
            Expression::Prefix(v) => v.string(),
            Expression::Slice(v) => v.string(),
            Expression::Map(v) => v.string(),
            Expression::Index(v) => v.string(),
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Expression::Bool(v) => v.position.to_owned(),
            Expression::Float(v) => v.position.to_owned(),
            Expression::Integer(v) => v.position.to_owned(),
            Expression::String(v) => v.position.to_owned(),
            Expression::Function(v) => v.position.to_owned(),
            Expression::Call(v) => v.position.to_owned(),
            Expression::Identifier(v) => v.position.to_owned(),
            Expression::If(v) => v.position.to_owned(),
            Expression::Infix(v) => v.position.to_owned(),
            Expression::Prefix(v) => v.position.to_owned(),
            Expression::Slice(v) => v.position.to_owned(),
            Expression::Map(v) => v.position.to_owned(),
            Expression::Index(v) => v.position.to_owned(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn string(&self) -> String {
        self.statements
            .iter()
            .map(|statement| statement.string())
            .reduce(|acc, e| acc + "\n" + e.as_str())
            .unwrap_or(String::default())
    }
}

#[derive(Clone)]
pub struct LetStatement {
    pub position: Position,
    pub name: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn string(&self) -> String {
        format!(
            "{} {} = {};",
            Token::LET.identifier(),
            self.name.string(),
            self.value.string()
        )
    }
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub position: Position,
    pub value: Expression,
}

impl ReturnStatement {
    pub fn string(&self) -> String {
        format!("{} {};", Token::RETURN.identifier(), self.value.string())
    }
}

#[derive(Clone)]
pub struct ExpressionStatement {
    pub position: Position,
    pub expression: Expression,
    /// if true, express statement value is empty value
    pub end_of_semicolon: bool,
}

impl ExpressionStatement {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        if self.end_of_semicolon {
            buf.push(';');
        }
        format!("{}{}", self.expression.string(), buf.as_str())
    }
}

#[derive(Clone)]
pub struct EmptyStatement {
    pub position: Position,
}

impl EmptyStatement {
    pub fn string(&self) -> String {
        ";".to_owned()
    }
}

#[derive(Clone)]
pub struct BlockStatement {
    pub position: Position,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::LBRACE.identifier());
        self.statements.iter().for_each(|statement| {
            buf.push('\n');
            buf.push('\t');
            buf.push_str(statement.string().as_str());
        });
        buf.push('\n');
        buf.push_str(Token::RBRACE.identifier());
        buf
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub position: Position,
    pub token: Token,
    pub right: Expression,
}

impl PrefixExpression {
    pub fn string(&self) -> String {
        format!("({}{})", self.token.literal(), self.right.string())
    }
}

#[derive(Clone)]
pub struct InfixExpression {
    pub position: Position,
    pub token: Token,
    pub left: Expression,
    pub right: Expression,
}

impl InfixExpression {
    pub fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.string(),
            self.token.literal(),
            self.right.string()
        )
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub position: Position,
    pub token: Token,
    pub name: String,
}

impl Identifier {
    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub position: Position,
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Clone)]
pub struct FloatLiteral {
    pub position: Position,
    pub token: Token,
    pub value: f64,
}

impl FloatLiteral {
    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Clone)]
pub struct BoolLiteral {
    pub position: Position,
    pub token: Token,
    pub value: bool,
}

impl BoolLiteral {
    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Clone)]
pub struct StringLiteral {
    pub position: Position,
    pub token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Clone)]
pub struct IfExpression {
    pub position: Position,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
    pub optional: Option<Box<Expression>>,
}

impl IfExpression {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::IF.identifier());
        buf.push(' ');
        buf.push_str(self.condition.string().as_str());
        buf.push(' ');
        buf.push_str(self.consequence.string().as_str());
        if self.alternative.is_some() {
            buf.push(' ');
            buf.push_str(Token::ELSE.identifier());
            buf.push(' ');
            buf.push_str(self.alternative.as_ref().unwrap().string().as_str());
        }
        if self.optional.is_some() {
            buf.push(' ');
            buf.push_str(Token::ELSE.identifier());
            buf.push(' ');
            buf.push_str(self.optional.as_ref().unwrap().string().as_str());
        }
        buf
    }
}

#[derive(Clone)]
pub struct WhileStatement {
    pub position: Position,
    pub condition: Expression,
    pub consequence: BlockStatement,
}

impl WhileStatement {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::WHILE.identifier());
        buf.push(' ');
        buf.push_str(self.condition.string().as_str());
        buf.push(' ');
        buf.push_str(self.consequence.string().as_str());
        buf
    }
}

#[derive(Clone)]
pub struct ForInStatement {
    pub position: Position,
    pub collection: Expression,
    pub key: Identifier,
    pub value: Option<Identifier>,
    pub body: BlockStatement,
}

impl ForInStatement {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::FOR.identifier());
        buf.push(' ');
        buf.push_str(self.key.string().as_str());
        if self.value.is_some() {
            buf.push_str(Token::COMMA.identifier());
            buf.push(' ');
            buf.push_str(self.key.string().as_str());
        }
        buf.push(' ');
        buf.push_str(Token::IN.identifier());
        buf.push(' ');
        buf.push_str(self.collection.string().as_str());
        buf.push(' ');
        buf.push_str(self.body.string().as_str());
        buf
    }
}

#[derive(Clone)]
pub struct IndexAssignStatement {
    pub position: Position,
    pub left: Expression,
    pub index: Expression,
    pub right: Expression,
}

impl IndexAssignStatement {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(self.left.string().as_str());
        buf.push_str(Token::LBRACKET.identifier());
        buf.push_str(self.index.string().as_str());
        buf.push_str(Token::RBRACKET.identifier());
        buf.push_str(self.right.string().as_str());
        buf
    }
}

#[derive(Clone)]
pub struct FunctionLiteral {
    pub position: Position,
    pub name: Option<Identifier>,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::FUNCTION.identifier());
        buf.push(' ');
        if self.name.is_some() {
            buf.push_str(self.name.as_ref().unwrap().string().as_str());
        }
        buf.push_str(Token::LPAREN.identifier());
        buf.push_str(
            self.parameters
                .iter()
                .map(|parameter| parameter.string())
                .reduce(|acc, e| acc + ", " + e.as_str())
                .unwrap_or(String::default())
                .as_str(),
        );
        buf.push_str(Token::RPAREN.identifier());
        buf.push(' ');
        buf.push_str(self.body.string().as_str());
        buf
    }
}

#[derive(Clone)]
pub struct CallExpression {
    pub position: Position,
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(self.function.string().as_str());
        buf.push_str(Token::LPAREN.identifier());
        buf.push_str(
            self.arguments
                .iter()
                .map(|parameter| parameter.string())
                .reduce(|acc, e| acc + ", " + e.as_str())
                .unwrap_or(String::default())
                .as_str(),
        );
        buf.push_str(Token::RPAREN.identifier());
        buf
    }
}

#[derive(Clone)]
pub struct MapLiteral {
    pub position: Position,
    pub kv_pair: Vec<(Expression, Expression)>,
}

impl MapLiteral {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::LBRACE.identifier());
        for (i, e) in self.kv_pair.iter().enumerate() {
            if i != 0 {
                buf.push_str(Token::COMMA.identifier());
                buf.push(' ');
            }
            buf.push_str(e.0.string().as_str());
            buf.push_str(Token::COLON.identifier());
            buf.push(' ');
            buf.push_str(e.1.string().as_str());
        }
        buf.push_str(Token::RBRACE.identifier());
        buf
    }
}

#[derive(Clone)]
pub struct SliceLiteral {
    pub position: Position,
    pub elements: Vec<Expression>,
}

impl SliceLiteral {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(Token::LBRACKET.identifier());
        for (i, e) in self.elements.iter().enumerate() {
            if i != 0 {
                buf.push_str(Token::COMMA.identifier());
                buf.push(' ');
            }
            buf.push_str(e.string().as_str());
        }
        buf.push_str(Token::RBRACKET.identifier());
        buf
    }
}

#[derive(Clone)]
pub struct IndexExpression {
    pub position: Position,
    pub left: Expression,
    pub index: Expression,
}

impl IndexExpression {
    pub fn string(&self) -> String {
        let mut buf = String::new();
        buf.push_str(self.left.string().as_str());
        buf.push_str(Token::LBRACKET.identifier());
        buf.push_str(self.index.string().as_str());
        buf.push_str(Token::RBRACKET.identifier());
        buf
    }
}
