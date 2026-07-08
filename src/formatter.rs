use crate::ast::{
    AssignTarget, BlockStatement, Expression, FunctionLiteral, IfExpression, Program, Statement,
};
use crate::error::Error;
use crate::lexer::new_lexer;
use crate::parser::new_parser;
use crate::token::Precedence;

const DEFAULT_LINE_WIDTH: usize = 88;

pub fn format_source(source: &str, path: Option<String>) -> Result<String, Error> {
    let mut parser = new_parser(new_lexer(source.to_string(), path));
    let program = parser.parse_program()?;
    Ok(PrettyFormatter::with_source(source).format_program(&program))
}

struct PrettyFormatter {
    output: String,
    indent: usize,
    line_width: usize,
    preserve_blank_lines: bool,
}

impl Default for PrettyFormatter {
    fn default() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            line_width: DEFAULT_LINE_WIDTH,
            preserve_blank_lines: false,
        }
    }
}

impl PrettyFormatter {
    fn with_source(_source: &str) -> Self {
        Self {
            preserve_blank_lines: true,
            ..Self::default()
        }
    }

    fn format_program(mut self, program: &Program) -> String {
        for (index, statement) in program.statements.iter().enumerate() {
            if index == 0 {
                self.write_indent();
            } else {
                self.write_statement_separator(&program.statements[index - 1], statement);
            }
            self.write_statement(statement);
        }
        self.output
    }

    fn write_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Block(block) => self.write_block(block),
            Statement::Empty(_) => self.write(";"),
            Statement::Expression(statement) => {
                self.write_expression(&statement.expression, Precedence::Lowest);
                if statement.end_of_semicolon {
                    self.write(";");
                }
            }
            Statement::Let(statement) => {
                self.write("let ");
                self.write(&statement.name.name);
                self.write(" = ");
                self.write_expression(&statement.value, Precedence::Lowest);
                self.write(";");
            }
            Statement::Return(statement) => {
                self.write("return ");
                self.write_expression(&statement.value, Precedence::Lowest);
                self.write(";");
            }
            Statement::Import(statement) => {
                self.write("import ");
                if let Some(alias) = &statement.alias {
                    self.write(&alias.name);
                    self.write(" from ");
                }
                self.write_string(&statement.path.value);
                self.write(";");
            }
            Statement::Export(statement) => {
                self.write("export ");
                self.write(&statement.name.name);
                self.write(";");
            }
            Statement::Struct(statement) => {
                self.write("struct ");
                self.write(&statement.name.name);
                self.write(" ");
                if statement.fields.is_empty() {
                    self.write("{}");
                    return;
                }
                self.write("{");
                self.indent += 1;
                for field in statement.fields.iter() {
                    self.newline();
                    self.write_indent();
                    self.write(&field.name);
                    self.write(";");
                }
                self.indent -= 1;
                self.newline();
                self.write_indent();
                self.write("}");
            }
            Statement::Break(_) => self.write("break;"),
            Statement::Continue(_) => self.write("continue;"),
            Statement::While(statement) => {
                self.write("while ");
                self.write_expression(&statement.condition, Precedence::Lowest);
                self.write(" ");
                self.write_block(&statement.consequence);
            }
            Statement::Assign(statement) => {
                self.write_assign_target(&statement.target);
                self.write(" = ");
                self.write_expression(&statement.value, Precedence::Lowest);
                self.write(";");
            }
            Statement::ForIn(statement) => {
                self.write("for ");
                self.write(&statement.key.name);
                if let Some(value) = &statement.value {
                    self.write(", ");
                    self.write(&value.name);
                }
                self.write(" in ");
                self.write_expression(&statement.collection, Precedence::Lowest);
                self.write(" ");
                self.write_block(&statement.body);
            }
        }
    }

    fn write_block(&mut self, block: &BlockStatement) {
        if block.statements.is_empty() {
            self.write("{}");
            return;
        }
        self.write("{");
        self.indent += 1;
        for (index, statement) in block.statements.iter().enumerate() {
            if index == 0 {
                self.newline();
                self.write_indent();
            } else {
                self.write_statement_separator(&block.statements[index - 1], statement);
            }
            self.write_statement(statement);
        }
        self.indent -= 1;
        self.newline();
        self.write_indent();
        self.write("}");
    }

    fn write_expression(&mut self, expression: &Expression, parent_precedence: Precedence) {
        let current_precedence = expression_precedence(expression);
        let needs_parens = current_precedence < parent_precedence;
        if needs_parens {
            self.write("(");
        }
        match expression {
            Expression::Bool(value) => self.write(if value.value { "true" } else { "false" }),
            Expression::Null(_) => self.write("null"),
            Expression::Float(value) => self.write(&value.value.to_string()),
            Expression::Integer(value) => self.write(&value.value.to_string()),
            Expression::String(value) => self.write_string(&value.value),
            Expression::Function(value) => self.write_function(value),
            Expression::Call(value) => {
                self.write_expression(&value.function, Precedence::Index);
                self.write("(");
                self.write_expression_list(&value.arguments);
                self.write(")");
            }
            Expression::Identifier(value) => self.write(&value.name),
            Expression::If(value) => self.write_if_expression(value),
            Expression::Infix(value) => {
                let precedence = value.token.precedence();
                self.write_expression(&value.left, precedence);
                self.write(" ");
                self.write(&value.token.literal());
                self.write(" ");
                let right_precedence = if value.token.is_assign() {
                    Precedence::Assign
                } else {
                    precedence.next()
                };
                self.write_expression(&value.right, right_precedence);
            }
            Expression::Prefix(value) => {
                self.write(&value.token.literal());
                self.write_expression(&value.right, Precedence::Prefix);
            }
            Expression::Grouped(value) => {
                self.write("(");
                self.write_expression(&value.expression, Precedence::Lowest);
                self.write(")");
            }
            Expression::Slice(value) => {
                let elements = value
                    .elements
                    .iter()
                    .map(|element| self.expression_to_string(element, Precedence::Lowest))
                    .collect::<Vec<_>>();
                self.write_delimited("[", "]", &elements);
            }
            Expression::Map(value) => {
                let entries = value
                    .kv_pair
                    .iter()
                    .map(|(key, value)| {
                        format!(
                            "{}: {}",
                            self.map_key_to_string(key),
                            self.expression_to_string(value, Precedence::Lowest)
                        )
                    })
                    .collect::<Vec<_>>();
                self.write_delimited("{", "}", &entries);
            }
            Expression::StructLiteral(value) => {
                self.write_expression(&value.name, Precedence::Index);
                let fields = if value.fields.is_empty() {
                    value
                        .values
                        .iter()
                        .map(|value| self.expression_to_string(value, Precedence::Lowest))
                        .collect::<Vec<_>>()
                } else {
                    value
                        .fields
                        .iter()
                        .map(|(name, value)| {
                            format!(
                                "{}: {}",
                                name.name,
                                self.expression_to_string(value, Precedence::Lowest)
                            )
                        })
                        .collect::<Vec<_>>()
                };
                self.write_delimited("{", "}", &fields);
            }
            Expression::Index(value) => {
                self.write_expression(&value.left, Precedence::Index);
                self.write("[");
                self.write_expression(&value.index, Precedence::Lowest);
                self.write("]");
            }
            Expression::Member(value) => {
                self.write_expression(&value.left, Precedence::Index);
                self.write(".");
                self.write(&value.property.name);
            }
        }
        if needs_parens {
            self.write(")");
        }
    }

    fn write_if_expression(&mut self, expression: &IfExpression) {
        self.write("if ");
        self.write_expression(&expression.condition, Precedence::Lowest);
        self.write(" ");
        self.write_block(&expression.consequence);
        if let Some(alternative) = &expression.alternative {
            self.write(" else ");
            self.write_block(alternative);
        } else if let Some(optional) = &expression.optional {
            self.write(" else ");
            self.write_expression(optional, Precedence::Lowest);
        }
    }

    fn write_function(&mut self, function: &FunctionLiteral) {
        self.write("fn");
        if let Some(name) = &function.name {
            self.write(" ");
            self.write(&name.name);
        }
        self.write("(");
        let parameters = function
            .parameters
            .iter()
            .map(|parameter| parameter.name.to_owned())
            .collect::<Vec<_>>();
        self.write(&parameters.join(", "));
        self.write(") ");
        self.write_block(&function.body);
    }

    fn write_assign_target(&mut self, target: &AssignTarget) {
        match target {
            AssignTarget::Identifier(value) => self.write(&value.name),
            AssignTarget::Index { left, index } => {
                self.write_expression(left, Precedence::Index);
                self.write("[");
                self.write_expression(index, Precedence::Lowest);
                self.write("]");
            }
            AssignTarget::Member { left, property } => {
                self.write_expression(left, Precedence::Index);
                self.write(".");
                self.write(&property.name);
            }
        }
    }

    fn write_expression_list(&mut self, expressions: &[Expression]) {
        let arguments = expressions
            .iter()
            .map(|expression| self.expression_to_string(expression, Precedence::Lowest))
            .collect::<Vec<_>>();
        self.write(&arguments.join(", "));
    }

    fn write_delimited(&mut self, open: &str, close: &str, entries: &[String]) {
        if entries.is_empty() {
            self.write(open);
            self.write(close);
            return;
        }

        let inline = format!("{open}{}{close}", entries.join(", "));
        if self.current_column() + inline.len() <= self.line_width
            && entries.iter().all(|entry| !entry.contains('\n'))
        {
            self.write(&inline);
            return;
        }

        self.write(open);
        self.indent += 1;
        for (index, entry) in entries.iter().enumerate() {
            self.newline();
            self.write_indent();
            self.write(entry);
            if index + 1 < entries.len() {
                self.write(",");
            }
        }
        self.indent -= 1;
        self.newline();
        self.write_indent();
        self.write(close);
    }

    fn expression_to_string(
        &self,
        expression: &Expression,
        parent_precedence: Precedence,
    ) -> String {
        let mut formatter = PrettyFormatter {
            output: String::new(),
            indent: self.indent,
            line_width: self.line_width,
            preserve_blank_lines: false,
        };
        formatter.write_expression(expression, parent_precedence);
        formatter.output
    }

    fn map_key_to_string(&self, expression: &Expression) -> String {
        match expression {
            Expression::String(value) if is_identifier_like(&value.value) => value.value.to_owned(),
            Expression::Grouped(value) => {
                format!(
                    "({})",
                    self.expression_to_string(&value.expression, Precedence::Lowest)
                )
            }
            _ => self.expression_to_string(expression, Precedence::Lowest),
        }
    }

    fn write_string(&mut self, value: &str) {
        self.write(&format!("{value:?}"));
    }

    fn write(&mut self, value: &str) {
        self.output.push_str(value);
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_statement_separator(&mut self, previous: &Statement, current: &Statement) {
        let mut newline_count = 1;
        if self.preserve_blank_lines {
            let previous_end_line = previous.span().end.0;
            let current_start_line = current.span().start.0;
            if current_start_line > previous_end_line + 1 {
                newline_count += current_start_line - previous_end_line - 1;
            }
        }
        for _ in 0..newline_count {
            self.newline();
        }
        self.write_indent();
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    fn current_column(&self) -> usize {
        self.output
            .rsplit('\n')
            .next()
            .map(|line| line.len())
            .unwrap_or(0)
    }
}

fn expression_precedence(expression: &Expression) -> Precedence {
    match expression {
        Expression::Infix(value) => value.token.precedence(),
        Expression::Prefix(_) => Precedence::Prefix,
        Expression::Grouped(_) => Precedence::Index,
        Expression::If(_) => Precedence::Lowest,
        _ => Precedence::Index,
    }
}

fn is_identifier_like(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

#[cfg(test)]
mod tests {
    use super::format_source;

    #[test]
    fn formats_statements_and_blocks_with_indentation() {
        let formatted = format_source(
            "fn add(x,y){let total=x+y;if total>3{return total;}else{return 0;}}",
            None,
        )
        .unwrap();
        assert_eq!(
            formatted,
            r#"fn add(x, y) {
    let total = x + y;
    if total > 3 {
        return total;
    } else {
        return 0;
    }
}"#
        );
    }

    #[test]
    fn formats_records_structs_and_imports() {
        let formatted = format_source(
            r#"import math from "lib.monkey";struct User{name;age;}let user=User{name:"tom",age:18};user.age=user.age+1;export user;"#,
            None,
        )
        .unwrap();
        assert_eq!(
            formatted,
            r#"import math from "lib.monkey";
struct User {
    name;
    age;
}
let user = User{name: "tom", age: 18};
user.age = user.age + 1;
export user;"#
        );
    }

    #[test]
    fn formats_long_collections_across_lines() {
        let formatted = format_source(
            r#"let data={first:"aaaaaaaaaaaaaaaaaaaa",second:"bbbbbbbbbbbbbbbbbbbb",third:"cccccccccccccccccccc",fourth:"dddddddddddddddddddd"};"#,
            None,
        )
        .unwrap();
        assert!(formatted.contains("let data = {"));
        assert!(formatted.contains("\n    first:"));
        assert!(formatted.contains("\n};"));
        assert!(format_source(&formatted, None).is_ok(), "{formatted}");
    }

    #[test]
    fn formats_boolean_operators_with_precedence() {
        let formatted = format_source("let ok=a==b&&c||d;let nested=a&&(b||c);", None).unwrap();
        assert_eq!(
            formatted,
            r#"let ok = a == b && c || d;
let nested = a && (b || c);"#
        );
    }

    #[test]
    fn preserves_blank_lines_and_grouping_parentheses() {
        let formatted = format_source("let a=(1+2)*3;\n\nlet b=a+(2*3);", None).unwrap();
        assert_eq!(
            formatted,
            r#"let a = (1 + 2) * 3;

let b = a + (2 * 3);"#
        );
    }

    #[test]
    fn formats_postfix_chains_with_shared_precedence() {
        let formatted = format_source(
            r#"let name=models.User{name:"tom",age:1}.profile.friends[0].name;let next=make_user().profile.name;"#,
            None,
        )
        .unwrap();
        assert_eq!(
            formatted,
            r#"let name = models.User{name: "tom", age: 1}.profile.friends[0].name;
let next = make_user().profile.name;"#
        );
    }

    #[test]
    fn formatting_is_idempotent() {
        let formatted = format_source(
            r#"
            import "stdlib/prelude.monkey";
            fn total(values){let sum=0;for i,value in values{sum=sum+value;}sum}
            let data={name:"tom",scores:[1,2,3]};
            data.total=total(data.scores);
            "#,
            None,
        )
        .unwrap();
        let formatted_again = format_source(&formatted, None).unwrap();
        assert_eq!(formatted_again, formatted);
    }
}
