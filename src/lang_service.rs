use crate::ast::{AssignTarget, Expression, FunctionLiteral, Program, Statement};
use crate::builtin::{new_builtin_function_map, new_global_builtin_function_map};
use crate::error::Error;
use crate::formatter::format_program;
use crate::lexer::{new_lexer, Span};
use crate::parser::new_parser;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

pub fn format_source_for_editor(source: &str, path: Option<String>) -> Result<String, Error> {
    let program = parse_program(source, path)?;
    Ok(format_program(&program))
}

pub fn diagnostics_json(source: &str, path: Option<String>) -> String {
    match parse_program(source, path) {
        Ok(_) => "{\"diagnostics\":[]}".to_string(),
        Err(err) => {
            let diagnostic = diagnostic_json(&err);
            format!("{{\"diagnostics\":[{diagnostic}]}}")
        }
    }
}

pub fn symbols_json(source: &str, path: Option<String>) -> String {
    let mut symbols = builtin_symbols();
    collect_keyword_symbols(&mut symbols);
    match parse_program(source, path) {
        Ok(program) => collect_program_symbols(&program, &mut symbols),
        Err(_) => collect_text_symbols(source, &mut symbols),
    }
    completions_json(symbols.values())
}

pub fn completion_json(
    source: &str,
    path: Option<String>,
    line: usize,
    character: usize,
) -> String {
    if let Some(context) = import_path_context(source, line, character) {
        let symbols = import_path_completions(path.as_deref(), &context)
            .into_iter()
            .map(|symbol| (symbol.label.clone(), symbol))
            .collect::<BTreeMap<_, _>>();
        return completions_json(symbols.values());
    }
    let Some(context) = member_context(source, line, character) else {
        return symbols_json(source, path);
    };
    let byte_offset = context.byte_offset;
    let repaired_source = format!(
        "{}__completion{}",
        &source[..byte_offset],
        &source[byte_offset..]
    );
    let mut completions = Vec::new();
    if let Ok(program) = parse_program(&repaired_source, path.clone()) {
        completions =
            member_completions(&program, path.as_deref(), &context.path, context.position);
    } else {
        let prefix_source = format!("{}__completion", &source[..byte_offset]);
        if let Ok(program) = parse_program(&prefix_source, path.clone()) {
            completions =
                member_completions(&program, path.as_deref(), &context.path, context.position);
        }
    }
    let symbols = completions
        .into_iter()
        .map(|symbol| (symbol.label.clone(), symbol))
        .collect::<BTreeMap<_, _>>();
    completions_json(symbols.values())
}

pub fn definition_json(
    source: &str,
    path: Option<String>,
    line: usize,
    character: usize,
) -> String {
    let Some(reference) = reference_at(source, line, character) else {
        return "{\"definition\":null}".to_string();
    };
    let Ok(program) = parse_program(source, path.clone()) else {
        return "{\"definition\":null}".to_string();
    };
    let position = SourcePosition::from_lsp(line, character);
    let definition = definition_for_reference(&program, path.as_deref(), &reference, position);
    match definition {
        Some(definition) => definition.to_json(),
        None => "{\"definition\":null}".to_string(),
    }
}

pub fn references_json(
    source: &str,
    path: Option<String>,
    line: usize,
    character: usize,
    include_declaration: bool,
) -> String {
    let Some(reference) = reference_at(source, line, character) else {
        return "{\"references\":[]}".to_string();
    };
    let Ok(program) = parse_program(source, path.clone()) else {
        return "{\"references\":[]}".to_string();
    };
    let position = SourcePosition::from_lsp(line, character);
    let Some(target) = reference_target(&reference) else {
        return "{\"references\":[]}".to_string();
    };
    let mut references = match &target {
        ReferenceTarget::Identifier(name) => {
            scoped_identifier_references(&program, name, position, include_declaration)
        }
        ReferenceTarget::MemberPath(_) => current_file_references(&program, &target),
    };
    references.sort_by_key(|reference| {
        (
            reference.path.clone(),
            reference.span.start.0,
            reference.span.start.1,
            reference.span.end.0,
            reference.span.end.1,
        )
    });
    references.dedup_by(|left, right| same_location(left, right));
    references_json_array(references.iter())
}

fn completions_json<'a>(symbols: impl Iterator<Item = &'a Symbol>) -> String {
    let completions = symbols
        .map(|symbol| {
            format!(
                "{{\"label\":\"{}\",\"kind\":\"{}\",\"detail\":\"{}\"}}",
                escape_json(&symbol.label),
                escape_json(symbol.kind.as_str()),
                escape_json(&symbol.detail)
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    format!("{{\"completions\":[{completions}]}}")
}

fn references_json_array<'a>(references: impl Iterator<Item = &'a Definition>) -> String {
    let references = references
        .map(Definition::location_json)
        .collect::<Vec<_>>()
        .join(",");
    format!("{{\"references\":[{references}]}}")
}

struct Reference {
    path: Vec<String>,
    word_index: usize,
}

fn reference_at(source: &str, line: usize, character: usize) -> Option<Reference> {
    let byte_offset = position_to_byte_offset(source, line, character)?;
    let line_start = source[..byte_offset]
        .rfind('\n')
        .map(|index| index + 1)
        .unwrap_or(0);
    let line_end = source[byte_offset..]
        .find('\n')
        .map(|index| byte_offset + index)
        .unwrap_or(source.len());
    let line_text = source.get(line_start..line_end)?;
    let relative_offset = byte_offset.saturating_sub(line_start);
    let word_range = identifier_range_at(line_text, relative_offset)?;

    let mut path_start = word_range.0;
    while path_start > 0 {
        let before = line_text.get(..path_start)?;
        if !before.ends_with('.') {
            break;
        }
        let receiver_end = path_start - 1;
        let receiver_range = identifier_range_ending_at(line_text, receiver_end)?;
        path_start = receiver_range.0;
    }

    let mut path_end = word_range.1;
    while path_end < line_text.len() {
        let after = line_text.get(path_end..)?;
        if !after.starts_with('.') {
            break;
        }
        let member_start = path_end + 1;
        let member_range = identifier_range_starting_at(line_text, member_start)?;
        path_end = member_range.1;
    }

    let mut word_index = None;
    let path = line_text
        .get(path_start..path_end)?
        .split('.')
        .enumerate()
        .scan(path_start, |start, (index, part)| {
            let end = *start + part.len();
            let range = (*start, end);
            *start = end + 1;
            Some((index, range, part))
        })
        .map(|(index, range, part)| {
            if range == word_range {
                word_index = Some(index);
            }
            let first = part.chars().next()?;
            if !is_identifier_start(first) || !part.chars().all(is_identifier_continue) {
                return None;
            }
            Some(part.to_string())
        })
        .collect::<Option<Vec<_>>>()?;
    if path.is_empty() {
        None
    } else {
        Some(Reference {
            path,
            word_index: word_index.unwrap_or(0),
        })
    }
}

fn identifier_range_at(line: &str, offset: usize) -> Option<(usize, usize)> {
    if offset > line.len() || !line.is_char_boundary(offset) {
        return None;
    }
    let mut start = offset;
    while start > 0 {
        let (index, ch) = line[..start].char_indices().next_back()?;
        if is_identifier_continue(ch) {
            start = index;
        } else {
            break;
        }
    }
    let mut end = offset;
    while end < line.len() {
        let ch = line[end..].chars().next()?;
        if is_identifier_continue(ch) {
            end += ch.len_utf8();
        } else {
            break;
        }
    }
    if start == end {
        return None;
    }
    let first = line[start..end].chars().next()?;
    is_identifier_start(first).then_some((start, end))
}

fn identifier_range_ending_at(line: &str, end: usize) -> Option<(usize, usize)> {
    if end == 0 || end > line.len() || !line.is_char_boundary(end) {
        return None;
    }
    let mut start = end;
    while start > 0 {
        let (index, ch) = line[..start].char_indices().next_back()?;
        if is_identifier_continue(ch) {
            start = index;
        } else {
            break;
        }
    }
    if start == end {
        return None;
    }
    let first = line[start..end].chars().next()?;
    is_identifier_start(first).then_some((start, end))
}

fn identifier_range_starting_at(line: &str, start: usize) -> Option<(usize, usize)> {
    if start >= line.len() || !line.is_char_boundary(start) {
        return None;
    }
    let first = line[start..].chars().next()?;
    if !is_identifier_start(first) {
        return None;
    }
    let mut end = start + first.len_utf8();
    while end < line.len() {
        let ch = line[end..].chars().next()?;
        if is_identifier_continue(ch) {
            end += ch.len_utf8();
        } else {
            break;
        }
    }
    Some((start, end))
}

fn parse_program(source: &str, path: Option<String>) -> Result<Program, Error> {
    let mut parser = new_parser(new_lexer(source.to_string(), path));
    parser.parse_program()
}

struct MemberContext {
    path: Vec<String>,
    byte_offset: usize,
    position: SourcePosition,
}

fn member_context(source: &str, line: usize, character: usize) -> Option<MemberContext> {
    let byte_offset = position_to_byte_offset(source, line, character)?;
    let prefix = source.get(..byte_offset)?;
    if !prefix.ends_with('.') {
        return None;
    }
    let path = member_path(prefix)?;
    if path.is_empty() {
        return None;
    }
    Some(MemberContext {
        path,
        byte_offset,
        position: SourcePosition::from_lsp(line, character),
    })
}

#[derive(Clone, Copy)]
struct SourcePosition {
    line: usize,
    character: usize,
}

impl SourcePosition {
    fn from_lsp(line: usize, character: usize) -> Self {
        Self {
            line: line + 1,
            character: character + 1,
        }
    }
}

fn member_path(prefix: &str) -> Option<Vec<String>> {
    let before_dot = prefix.strip_suffix('.')?;
    let mut start = before_dot.len();
    for (index, ch) in before_dot.char_indices().rev() {
        if is_identifier_continue(ch) || ch == '.' {
            start = index;
        } else {
            break;
        }
    }
    before_dot
        .get(start..)?
        .split('.')
        .map(|part| {
            let first = part.chars().next()?;
            if !is_identifier_start(first) || !part.chars().all(is_identifier_continue) {
                return None;
            }
            Some(part.to_string())
        })
        .collect()
}

fn position_to_byte_offset(source: &str, line: usize, character: usize) -> Option<usize> {
    let mut line_start = 0;
    for (current_line, segment) in source.split_inclusive('\n').enumerate() {
        if current_line == line {
            return utf16_character_to_byte_offset(line_start, segment, character);
        }
        line_start += segment.len();
    }
    if line == source.lines().count() {
        return Some(source.len());
    }
    None
}

fn utf16_character_to_byte_offset(
    line_start: usize,
    line_text: &str,
    character: usize,
) -> Option<usize> {
    let mut utf16_units = 0;
    for (byte_index, ch) in line_text.char_indices() {
        if utf16_units == character {
            return Some(line_start + byte_index);
        }
        utf16_units += ch.len_utf16();
        if utf16_units > character {
            return None;
        }
    }
    if utf16_units == character {
        Some(
            line_start
                + line_text
                    .trim_end_matches('\n')
                    .trim_end_matches('\r')
                    .len(),
        )
    } else {
        None
    }
}

fn is_identifier_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_identifier_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

struct ImportPathContext {
    prefix: String,
}

fn import_path_context(source: &str, line: usize, character: usize) -> Option<ImportPathContext> {
    let byte_offset = position_to_byte_offset(source, line, character)?;
    let line_start = source[..byte_offset]
        .rfind('\n')
        .map(|index| index + 1)
        .unwrap_or(0);
    let line_prefix = source.get(line_start..byte_offset)?;
    let quote_index = line_prefix.rfind('"')?;
    if line_prefix[..quote_index].matches('"').count() % 2 != 0 {
        return None;
    }
    let before_quote = line_prefix[..quote_index].trim_start();
    if !before_quote.starts_with("import") {
        return None;
    }
    Some(ImportPathContext {
        prefix: line_prefix[quote_index + 1..].to_string(),
    })
}

fn import_path_completions(current_path: Option<&str>, context: &ImportPathContext) -> Vec<Symbol> {
    let mut completions = BTreeMap::new();
    for root in import_roots(current_path) {
        let dir_part = Path::new(&context.prefix)
            .parent()
            .map(|path| path.to_path_buf())
            .unwrap_or_default();
        let partial = Path::new(&context.prefix)
            .file_name()
            .map(|value| value.to_string_lossy().into_owned())
            .unwrap_or_default();
        let base = root.join(&dir_part);
        let Ok(entries) = fs::read_dir(base.as_path()) else {
            continue;
        };
        for entry in entries.flatten() {
            let file_name = entry.file_name().to_string_lossy().into_owned();
            if !file_name.starts_with(&partial) {
                continue;
            }
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                let label = format!("{file_name}/");
                completions.insert(
                    label.clone(),
                    Symbol {
                        label,
                        kind: SymbolKind::Folder,
                        detail: "directory".to_string(),
                    },
                );
            } else if file_name.ends_with(".monkey") {
                let detail = import_file_detail(entry.path().as_path());
                completions.insert(
                    file_name.clone(),
                    Symbol {
                        label: file_name,
                        kind: SymbolKind::File,
                        detail,
                    },
                );
            }
        }
    }
    completions.into_values().collect()
}

fn import_roots(current_path: Option<&str>) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(current_path) = current_path {
        if let Some(parent) = Path::new(current_path).parent() {
            roots.push(parent.to_path_buf());
        }
    }
    if let Ok(cwd) = std::env::current_dir() {
        if !roots.iter().any(|root| root == &cwd) {
            roots.push(cwd);
        }
    }
    roots
}

fn import_file_detail(path: &Path) -> String {
    let Ok(source) = fs::read_to_string(path) else {
        return "Monkey file".to_string();
    };
    let Ok(program) = parse_program(&source, Some(path.to_string_lossy().into_owned())) else {
        return "Monkey file".to_string();
    };
    let exports = program
        .statements
        .iter()
        .filter_map(|statement| match statement {
            Statement::Export(statement) => Some(statement.name.name.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();
    if exports.is_empty() {
        "Monkey file".to_string()
    } else {
        format!("exports {}", exports.join(", "))
    }
}

#[derive(Clone)]
struct Definition {
    path: Option<String>,
    span: Span,
}

impl Definition {
    fn location_json(&self) -> String {
        let (start_line, start_character, end_line, end_character) = lsp_range(self.span);
        let path = self
            .path
            .as_ref()
            .map(|path| format!("\"{}\"", escape_json(path)))
            .unwrap_or_else(|| "null".to_string());
        format!(
            "{{\"path\":{path},\"range\":{{\"start\":{{\"line\":{},\"character\":{}}},\"end\":{{\"line\":{},\"character\":{}}}}}}}",
            start_line, start_character, end_line, end_character
        )
    }

    fn to_json(&self) -> String {
        format!("{{\"definition\":{}}}", self.location_json())
    }
}

enum ReferenceTarget {
    Identifier(String),
    MemberPath(Vec<String>),
}

fn reference_target(reference: &Reference) -> Option<ReferenceTarget> {
    if reference.path.len() == 1 || reference.word_index == 0 {
        return reference
            .path
            .get(reference.word_index)
            .cloned()
            .map(ReferenceTarget::Identifier);
    }
    Some(ReferenceTarget::MemberPath(
        reference.path.get(..=reference.word_index)?.to_vec(),
    ))
}

fn definition_for_reference(
    program: &Program,
    current_path: Option<&str>,
    reference: &Reference,
    position: SourcePosition,
) -> Option<Definition> {
    if reference.path.len() == 1 || reference.word_index == 0 {
        let name = reference.path[0].as_str();
        if let Some(definition) = local_definition(program, name, position) {
            return Some(definition);
        }
        if let Some(definition) = import_alias_definition(program, current_path, name, position) {
            return Some(definition);
        }
        return bare_import_definition(program, current_path, name, position);
    }
    let receiver = reference.path.first()?;
    let member = reference.path.get(reference.word_index)?;
    namespace_import_definition(program, current_path, receiver, member, position)
}

fn local_definition(program: &Program, name: &str, position: SourcePosition) -> Option<Definition> {
    let mut definitions = Vec::new();
    for statement in &program.statements {
        collect_local_definitions(statement, name, Some(position), &mut definitions);
    }
    definitions
        .into_iter()
        .filter(|definition| span_starts_before_or_at(definition.span, position))
        .max_by_key(|definition| {
            (
                definition.span.start.0,
                definition.span.start.1,
                definition.span.end.0,
                definition.span.end.1,
            )
        })
}

fn collect_local_definitions(
    statement: &Statement,
    name: &str,
    position: Option<SourcePosition>,
    definitions: &mut Vec<Definition>,
) {
    if statement_starts_after(statement, position) {
        return;
    }
    match statement {
        Statement::Let(statement) if statement.name.name == name => {
            definitions.push(current_file_definition(statement.name.span));
        }
        Statement::Struct(statement) if statement.name.name == name => {
            definitions.push(current_file_definition(statement.name.span));
        }
        Statement::Expression(statement) => {
            if let Expression::Function(function) = &statement.expression {
                if let Some(function_name) = &function.name {
                    if function_name.name == name {
                        definitions.push(current_file_definition(function_name.span));
                    }
                }
                for parameter in &function.parameters {
                    if parameter.name == name {
                        definitions.push(current_file_definition(parameter.span));
                    }
                }
                for statement in &function.body.statements {
                    collect_local_definitions(statement, name, position, definitions);
                }
            }
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                collect_local_definitions(statement, name, position, definitions);
            }
        }
        Statement::While(statement) => {
            for statement in &statement.consequence.statements {
                collect_local_definitions(statement, name, position, definitions);
            }
        }
        Statement::ForIn(statement) => {
            if statement.key.name == name {
                definitions.push(current_file_definition(statement.key.span));
            }
            if let Some(value) = &statement.value {
                if value.name == name {
                    definitions.push(current_file_definition(value.span));
                }
            }
            for statement in &statement.body.statements {
                collect_local_definitions(statement, name, position, definitions);
            }
        }
        _ => {}
    }
}

fn current_file_definition(span: Span) -> Definition {
    Definition { path: None, span }
}

fn span_starts_before_or_at(span: Span, position: SourcePosition) -> bool {
    span.start.0 < position.line
        || (span.start.0 == position.line && span.start.1 <= position.character)
}

fn import_alias_definition(
    program: &Program,
    current_path: Option<&str>,
    name: &str,
    position: SourcePosition,
) -> Option<Definition> {
    let import = program
        .statements
        .iter()
        .filter(|statement| !statement_starts_after(statement, Some(position)))
        .find_map(|statement| match statement {
            Statement::Import(statement) => statement
                .alias
                .as_ref()
                .filter(|alias| alias.name == name)
                .map(|_| statement),
            _ => None,
        })?;
    let path = resolve_import_path(current_path, import.path.value.as_str())?;
    Some(Definition {
        path: Some(path.to_string_lossy().into_owned()),
        span: Span::point(crate::lexer::Position(1, 1)),
    })
}

fn bare_import_definition(
    program: &Program,
    current_path: Option<&str>,
    name: &str,
    position: SourcePosition,
) -> Option<Definition> {
    for (alias, import_path) in imports(program, Some(position)) {
        if alias.is_some() {
            continue;
        }
        let Some(definition) = module_export_definition(current_path, import_path.as_str(), name)
        else {
            continue;
        };
        return Some(definition);
    }
    None
}

fn namespace_import_definition(
    program: &Program,
    current_path: Option<&str>,
    receiver: &str,
    member: &str,
    position: SourcePosition,
) -> Option<Definition> {
    let import_path = program
        .statements
        .iter()
        .filter(|statement| !statement_starts_after(statement, Some(position)))
        .find_map(|statement| match statement {
            Statement::Import(statement) => statement
                .alias
                .as_ref()
                .filter(|alias| alias.name == receiver)
                .map(|_| statement.path.value.to_string()),
            _ => None,
        })?;
    module_export_definition(current_path, import_path.as_str(), member)
}

fn module_export_definition(
    current_path: Option<&str>,
    import_path: &str,
    name: &str,
) -> Option<Definition> {
    let path = resolve_import_path(current_path, import_path)?;
    let source = fs::read_to_string(path.as_path()).ok()?;
    let program = parse_program(&source, Some(path.to_string_lossy().into_owned())).ok()?;
    let exported = program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::Export(statement) if statement.name.name == name => {
                Some(statement.name.span)
            }
            _ => None,
        })?;
    if let Some(definition_span) = declaration_span(&program, name) {
        return Some(Definition {
            path: Some(path.to_string_lossy().into_owned()),
            span: definition_span,
        });
    }
    Some(Definition {
        path: Some(path.to_string_lossy().into_owned()),
        span: exported,
    })
}

fn declaration_span(program: &Program, name: &str) -> Option<Span> {
    for statement in &program.statements {
        if let Some(span) = declaration_span_in_statement(statement, name) {
            return Some(span);
        }
    }
    None
}

fn declaration_span_in_statement(statement: &Statement, name: &str) -> Option<Span> {
    match statement {
        Statement::Let(statement) if statement.name.name == name => Some(statement.name.span),
        Statement::Struct(statement) if statement.name.name == name => Some(statement.name.span),
        Statement::Expression(statement) => {
            if let Expression::Function(function) = &statement.expression {
                if let Some(function_name) = &function.name {
                    if function_name.name == name {
                        return Some(function_name.span);
                    }
                }
            }
            None
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                if let Some(span) = declaration_span_in_statement(statement, name) {
                    return Some(span);
                }
            }
            None
        }
        _ => None,
    }
}

#[derive(Clone)]
struct IdentifierOccurrence {
    name: String,
    span: Span,
    declaration: Option<Span>,
    is_declaration: bool,
}

struct IdentifierReferenceCollector {
    scopes: Vec<BTreeMap<String, Span>>,
    occurrences: Vec<IdentifierOccurrence>,
}

impl IdentifierReferenceCollector {
    fn new() -> Self {
        Self {
            scopes: vec![BTreeMap::new()],
            occurrences: Vec::new(),
        }
    }

    fn collect_statements(&mut self, statements: &[Statement]) {
        for statement in statements {
            self.collect_statement(statement);
        }
    }

    fn collect_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Block(block) => {
                self.with_scope(|collector| collector.collect_statements(&block.statements));
            }
            Statement::Expression(statement) => {
                self.collect_expression(&statement.expression);
            }
            Statement::Let(statement) => {
                if matches!(statement.value, Expression::Function(_)) {
                    self.declare(&statement.name);
                    self.collect_expression(&statement.value);
                } else {
                    self.collect_expression(&statement.value);
                    self.declare(&statement.name);
                }
            }
            Statement::Return(statement) => {
                self.collect_expression(&statement.value);
            }
            Statement::Import(statement) => {
                if let Some(alias) = &statement.alias {
                    self.declare(alias);
                }
            }
            Statement::Export(statement) => {
                self.use_identifier(&statement.name);
            }
            Statement::Struct(statement) => {
                self.declare(&statement.name);
            }
            Statement::While(statement) => {
                self.collect_expression(&statement.condition);
                self.collect_statements(&statement.consequence.statements);
            }
            Statement::Assign(statement) => {
                self.collect_assign_target(&statement.target);
                self.collect_expression(&statement.value);
            }
            Statement::ForIn(statement) => {
                self.collect_expression(&statement.collection);
                self.declare(&statement.key);
                if let Some(value) = &statement.value {
                    self.declare(value);
                }
                self.with_scope(|collector| {
                    collector.collect_statements(&statement.body.statements)
                });
            }
            Statement::Empty(_) | Statement::Break(_) | Statement::Continue(_) => {}
        }
    }

    fn collect_assign_target(&mut self, target: &AssignTarget) {
        match target {
            AssignTarget::Identifier(identifier) => self.use_identifier(identifier),
            AssignTarget::Index { left, index } => {
                self.collect_expression(left);
                self.collect_expression(index);
            }
            AssignTarget::Member { left, .. } => {
                self.collect_expression(left);
            }
        }
    }

    fn collect_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Function(function) => {
                if let Some(name) = &function.name {
                    self.declare(name);
                }
                self.with_scope(|collector| {
                    for parameter in &function.parameters {
                        collector.declare(parameter);
                    }
                    collector.collect_statements(&function.body.statements);
                });
            }
            Expression::Call(call) => {
                self.collect_expression(&call.function);
                for argument in &call.arguments {
                    self.collect_expression(argument);
                }
            }
            Expression::Identifier(identifier) => self.use_identifier(identifier),
            Expression::If(expression) => {
                self.collect_expression(&expression.condition);
                self.collect_statements(&expression.consequence.statements);
                if let Some(alternative) = &expression.alternative {
                    self.collect_statements(&alternative.statements);
                }
                if let Some(optional) = &expression.optional {
                    self.collect_expression(optional);
                }
            }
            Expression::Infix(expression) => {
                self.collect_expression(&expression.left);
                self.collect_expression(&expression.right);
            }
            Expression::Prefix(expression) => {
                self.collect_expression(&expression.right);
            }
            Expression::Slice(slice) => {
                for element in &slice.elements {
                    self.collect_expression(element);
                }
            }
            Expression::Map(map) => {
                for (key, value) in &map.kv_pair {
                    self.collect_expression(key);
                    self.collect_expression(value);
                }
            }
            Expression::StructLiteral(literal) => {
                self.collect_expression(&literal.name);
                for (_, value) in &literal.fields {
                    self.collect_expression(value);
                }
                for value in &literal.values {
                    self.collect_expression(value);
                }
            }
            Expression::Index(expression) => {
                self.collect_expression(&expression.left);
                self.collect_expression(&expression.index);
            }
            Expression::Member(expression) => {
                self.collect_expression(&expression.left);
            }
            Expression::Bool(_)
            | Expression::Null(_)
            | Expression::Float(_)
            | Expression::Integer(_)
            | Expression::String(_) => {}
        }
    }

    fn declare(&mut self, identifier: &crate::ast::Identifier) {
        self.occurrences.push(IdentifierOccurrence {
            name: identifier.name.to_string(),
            span: identifier.span,
            declaration: Some(identifier.span),
            is_declaration: true,
        });
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(identifier.name.to_string(), identifier.span);
        }
    }

    fn use_identifier(&mut self, identifier: &crate::ast::Identifier) {
        self.occurrences.push(IdentifierOccurrence {
            name: identifier.name.to_string(),
            span: identifier.span,
            declaration: self.resolve(identifier.name.as_str()),
            is_declaration: false,
        });
    }

    fn resolve(&self, name: &str) -> Option<Span> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn with_scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.scopes.push(BTreeMap::new());
        f(self);
        self.scopes.pop();
    }
}

fn scoped_identifier_references(
    program: &Program,
    name: &str,
    position: SourcePosition,
    include_declaration: bool,
) -> Vec<Definition> {
    let mut collector = IdentifierReferenceCollector::new();
    collector.collect_statements(&program.statements);
    let target_declaration = collector
        .occurrences
        .iter()
        .find(|occurrence| {
            occurrence.name == name && span_contains_position(occurrence.span, Some(position))
        })
        .and_then(|occurrence| occurrence.declaration);
    collector
        .occurrences
        .into_iter()
        .filter(|occurrence| {
            occurrence.name == name && occurrence.declaration == target_declaration
        })
        .filter(|occurrence| include_declaration || !occurrence.is_declaration)
        .map(|occurrence| current_file_definition(occurrence.span))
        .collect()
}

fn current_file_references(program: &Program, target: &ReferenceTarget) -> Vec<Definition> {
    let mut references = Vec::new();
    for statement in &program.statements {
        collect_statement_references(statement, target, &mut references);
    }
    references
}

fn collect_statement_references(
    statement: &Statement,
    target: &ReferenceTarget,
    references: &mut Vec<Definition>,
) {
    match statement {
        Statement::Block(block) => {
            for statement in &block.statements {
                collect_statement_references(statement, target, references);
            }
        }
        Statement::Expression(statement) => {
            collect_expression_references(&statement.expression, target, references);
        }
        Statement::Let(statement) => {
            push_identifier_reference(&statement.name, target, references);
            collect_expression_references(&statement.value, target, references);
        }
        Statement::Return(statement) => {
            collect_expression_references(&statement.value, target, references);
        }
        Statement::Import(statement) => {
            if let Some(alias) = &statement.alias {
                push_identifier_reference(alias, target, references);
            }
        }
        Statement::Export(statement) => {
            push_identifier_reference(&statement.name, target, references);
        }
        Statement::Struct(statement) => {
            push_identifier_reference(&statement.name, target, references);
        }
        Statement::While(statement) => {
            collect_expression_references(&statement.condition, target, references);
            for statement in &statement.consequence.statements {
                collect_statement_references(statement, target, references);
            }
        }
        Statement::Assign(statement) => {
            collect_assign_target_references(&statement.target, target, references);
            collect_expression_references(&statement.value, target, references);
        }
        Statement::ForIn(statement) => {
            collect_expression_references(&statement.collection, target, references);
            push_identifier_reference(&statement.key, target, references);
            if let Some(value) = &statement.value {
                push_identifier_reference(value, target, references);
            }
            for statement in &statement.body.statements {
                collect_statement_references(statement, target, references);
            }
        }
        Statement::Empty(_) | Statement::Break(_) | Statement::Continue(_) => {}
    }
}

fn collect_assign_target_references(
    target: &AssignTarget,
    reference_target: &ReferenceTarget,
    references: &mut Vec<Definition>,
) {
    match target {
        AssignTarget::Identifier(identifier) => {
            push_identifier_reference(identifier, reference_target, references);
        }
        AssignTarget::Index { left, index } => {
            collect_expression_references(left, reference_target, references);
            collect_expression_references(index, reference_target, references);
        }
        AssignTarget::Member { left, property } => {
            if let Some(path) = member_path_from_parts(left, property) {
                push_member_path_reference(&path, reference_target, references);
            }
            collect_expression_references(left, reference_target, references);
        }
    }
}

fn collect_expression_references(
    expression: &Expression,
    target: &ReferenceTarget,
    references: &mut Vec<Definition>,
) {
    match expression {
        Expression::Function(function) => {
            if let Some(name) = &function.name {
                push_identifier_reference(name, target, references);
            }
            for parameter in &function.parameters {
                push_identifier_reference(parameter, target, references);
            }
            for statement in &function.body.statements {
                collect_statement_references(statement, target, references);
            }
        }
        Expression::Call(call) => {
            collect_expression_references(&call.function, target, references);
            for argument in &call.arguments {
                collect_expression_references(argument, target, references);
            }
        }
        Expression::Identifier(identifier) => {
            push_identifier_reference(identifier, target, references);
        }
        Expression::If(expression) => {
            collect_expression_references(&expression.condition, target, references);
            for statement in &expression.consequence.statements {
                collect_statement_references(statement, target, references);
            }
            if let Some(alternative) = &expression.alternative {
                for statement in &alternative.statements {
                    collect_statement_references(statement, target, references);
                }
            }
            if let Some(optional) = &expression.optional {
                collect_expression_references(optional, target, references);
            }
        }
        Expression::Infix(expression) => {
            collect_expression_references(&expression.left, target, references);
            collect_expression_references(&expression.right, target, references);
        }
        Expression::Prefix(expression) => {
            collect_expression_references(&expression.right, target, references);
        }
        Expression::Slice(slice) => {
            for element in &slice.elements {
                collect_expression_references(element, target, references);
            }
        }
        Expression::Map(map) => {
            for (key, value) in &map.kv_pair {
                collect_expression_references(key, target, references);
                collect_expression_references(value, target, references);
            }
        }
        Expression::StructLiteral(literal) => {
            collect_expression_references(&literal.name, target, references);
            for (_, value) in &literal.fields {
                collect_expression_references(value, target, references);
            }
            for value in &literal.values {
                collect_expression_references(value, target, references);
            }
        }
        Expression::Index(expression) => {
            collect_expression_references(&expression.left, target, references);
            collect_expression_references(&expression.index, target, references);
        }
        Expression::Member(expression) => {
            if let Some(path) = member_path_from_parts(&expression.left, &expression.property) {
                push_member_path_reference(&path, target, references);
            }
            collect_expression_references(&expression.left, target, references);
        }
        Expression::Bool(_)
        | Expression::Null(_)
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::String(_) => {}
    }
}

fn push_identifier_reference(
    identifier: &crate::ast::Identifier,
    target: &ReferenceTarget,
    references: &mut Vec<Definition>,
) {
    if let ReferenceTarget::Identifier(name) = target {
        if identifier.name == *name {
            references.push(current_file_definition(identifier.span));
        }
    }
}

fn push_member_path_reference(
    path: &[crate::ast::Identifier],
    target: &ReferenceTarget,
    references: &mut Vec<Definition>,
) {
    let ReferenceTarget::MemberPath(target_path) = target else {
        return;
    };
    if path.len() != target_path.len() {
        return;
    }
    if path
        .iter()
        .zip(target_path)
        .all(|(identifier, name)| identifier.name == *name)
    {
        if let Some(identifier) = path.last() {
            references.push(current_file_definition(identifier.span));
        }
    }
}

fn member_path_from_parts(
    left: &Expression,
    property: &crate::ast::Identifier,
) -> Option<Vec<crate::ast::Identifier>> {
    let mut path = expression_member_path(left)?;
    path.push(property.clone());
    Some(path)
}

fn expression_member_path(expression: &Expression) -> Option<Vec<crate::ast::Identifier>> {
    match expression {
        Expression::Identifier(identifier) => Some(vec![identifier.clone()]),
        Expression::Member(expression) => {
            member_path_from_parts(&expression.left, &expression.property)
        }
        _ => None,
    }
}

fn same_location(left: &Definition, right: &Definition) -> bool {
    left.path == right.path && left.span == right.span
}

#[derive(Clone, PartialEq, Eq)]
enum ValueShape {
    Result(Box<ValueShape>),
    Error,
    HttpResponse,
    ExecResult,
    Metadata,
    TerminalKey,
    TerminalSize,
    Struct(String),
    Unknown,
}

fn member_completions(
    program: &Program,
    path: Option<&str>,
    member_path: &[String],
    position: SourcePosition,
) -> Vec<Symbol> {
    if member_path.is_empty() {
        return Vec::new();
    }
    let struct_fields = collect_visible_struct_fields(program, path, Some(position));
    if let Some(completions) =
        shape_member_completions(program, &struct_fields, member_path, position)
    {
        return completions;
    }
    if member_path.len() != 1 {
        return Vec::new();
    }
    let receiver = &member_path[0];
    if let Some(completions) = module_member_completions(program, path, receiver, position) {
        return completions;
    }
    let variable_types = collect_variable_struct_types(program, Some(position));
    let Some(struct_name) = variable_types.get(receiver) else {
        return Vec::new();
    };
    struct_fields
        .get(struct_name)
        .map(|fields| {
            fields
                .iter()
                .map(|field| Symbol {
                    label: field.to_string(),
                    kind: SymbolKind::Field,
                    detail: format!("field {struct_name}.{field}"),
                })
                .collect()
        })
        .unwrap_or_default()
}

fn shape_member_completions(
    program: &Program,
    struct_fields: &BTreeMap<String, Vec<String>>,
    member_path: &[String],
    position: SourcePosition,
) -> Option<Vec<Symbol>> {
    let variable_shapes = collect_variable_shapes(program, Some(position));
    let shape = resolve_shape_path(member_path, &variable_shapes, struct_fields)?;
    Some(shape.completions(struct_fields))
}

impl ValueShape {
    fn member_shape(
        &self,
        property: &str,
        struct_fields: &BTreeMap<String, Vec<String>>,
    ) -> Option<ValueShape> {
        match self {
            ValueShape::Result(value) => match property {
                "value" => Some((**value).clone()),
                "error" => Some(ValueShape::Error),
                _ => None,
            },
            ValueShape::Struct(name) => struct_fields
                .get(name)
                .filter(|fields| fields.iter().any(|field| field == property))
                .map(|_| ValueShape::Unknown),
            _ => None,
        }
    }

    fn completions(&self, struct_fields: &BTreeMap<String, Vec<String>>) -> Vec<Symbol> {
        match self {
            ValueShape::Result(value) => vec![
                field_symbol("ok", "bool", "whether the call completed successfully"),
                field_symbol("value", value.detail_name(), "successful value"),
                field_symbol("error", "Error", "failure details"),
            ],
            ValueShape::Error => vec![
                field_symbol("kind", "string", "error category"),
                field_symbol("code", "string", "machine-readable error code"),
                field_symbol("message", "string", "human-readable error message"),
            ],
            ValueShape::HttpResponse => vec![
                field_symbol("status", "int", "HTTP status code"),
                field_symbol("status_ok", "bool", "true for 2xx and 3xx responses"),
                field_symbol("headers", "map", "response headers"),
                field_symbol("body", "string", "response body"),
            ],
            ValueShape::ExecResult => vec![
                field_symbol("success", "bool", "true when exit status is zero"),
                field_symbol("status", "int", "process exit status"),
                field_symbol("stdout", "string", "captured stdout"),
                field_symbol("stderr", "string", "captured stderr"),
            ],
            ValueShape::Metadata => vec![
                field_symbol("exists", "bool", "whether the path exists"),
                field_symbol("is_file", "bool", "whether the path is a file"),
                field_symbol("is_dir", "bool", "whether the path is a directory"),
                field_symbol("size", "int", "file size in bytes"),
                field_symbol("modified_ms", "int", "last modified time in milliseconds"),
            ],
            ValueShape::TerminalKey => vec![
                field_symbol("kind", "string", "stable key category"),
                field_symbol("key", "string", "key name or typed character"),
                field_symbol("ctrl", "bool", "whether Control was held"),
                field_symbol("alt", "bool", "whether Alt was held"),
                field_symbol("shift", "bool", "whether Shift was held"),
            ],
            ValueShape::TerminalSize => vec![
                field_symbol("cols", "int", "terminal width in columns"),
                field_symbol("rows", "int", "terminal height in rows"),
            ],
            ValueShape::Struct(name) => struct_fields
                .get(name)
                .map(|fields| {
                    fields
                        .iter()
                        .map(|field| Symbol {
                            label: field.to_string(),
                            kind: SymbolKind::Field,
                            detail: format!("field {name}.{field}"),
                        })
                        .collect()
                })
                .unwrap_or_default(),
            ValueShape::Unknown => Vec::new(),
        }
    }

    fn detail_name(&self) -> &'static str {
        match self {
            ValueShape::Result(_) => "Result",
            ValueShape::Error => "Error",
            ValueShape::HttpResponse => "HttpResponse",
            ValueShape::ExecResult => "ExecResult",
            ValueShape::Metadata => "Metadata",
            ValueShape::TerminalKey => "TerminalKey",
            ValueShape::TerminalSize => "TerminalSize",
            ValueShape::Struct(_) => "struct",
            ValueShape::Unknown => "value",
        }
    }

    fn signature_name(&self) -> String {
        match self {
            ValueShape::Result(value) => format!("Result<{}>", value.signature_name()),
            ValueShape::Error => "Error".to_string(),
            ValueShape::HttpResponse => "{status, status_ok, headers, body}".to_string(),
            ValueShape::ExecResult => "{success, status, stdout, stderr}".to_string(),
            ValueShape::Metadata => "{exists, is_file, is_dir, size, modified_ms}".to_string(),
            ValueShape::TerminalKey => "{kind, key, ctrl, alt, shift}".to_string(),
            ValueShape::TerminalSize => "{cols, rows}".to_string(),
            ValueShape::Struct(name) => name.to_string(),
            ValueShape::Unknown => "value".to_string(),
        }
    }
}

fn field_symbol(name: &str, typ: &str, description: &str) -> Symbol {
    Symbol {
        label: name.to_string(),
        kind: SymbolKind::Field,
        detail: format!("field {name}: {typ} - {description}"),
    }
}

struct ShapeContext {
    functions: BTreeMap<String, FunctionLiteral>,
    function_returns: BTreeMap<String, Option<ValueShape>>,
    call_stack: Vec<String>,
}

impl ShapeContext {
    fn new(program: &Program, position: Option<SourcePosition>) -> Self {
        Self {
            functions: collect_named_functions(program, position),
            function_returns: BTreeMap::new(),
            call_stack: Vec::new(),
        }
    }

    fn function_return_shape(&mut self, name: &str) -> Option<ValueShape> {
        if let Some(shape) = self.function_returns.get(name) {
            return shape.clone();
        }
        if self.call_stack.iter().any(|entry| entry == name) {
            self.function_returns.insert(name.to_string(), None);
            return None;
        }
        let function = self.functions.get(name)?.clone();
        self.call_stack.push(name.to_string());
        let shape = infer_function_return_shape(&function, self);
        self.call_stack.pop();
        self.function_returns
            .insert(name.to_string(), shape.clone());
        shape
    }
}

fn collect_named_functions(
    program: &Program,
    position: Option<SourcePosition>,
) -> BTreeMap<String, FunctionLiteral> {
    let mut functions = BTreeMap::new();
    for statement in &program.statements {
        collect_statement_named_functions(statement, &mut functions, position);
    }
    functions
}

fn collect_statement_named_functions(
    statement: &Statement,
    functions: &mut BTreeMap<String, FunctionLiteral>,
    position: Option<SourcePosition>,
) {
    if statement_starts_after(statement, position) {
        return;
    }
    match statement {
        Statement::Let(statement) => {
            if let Expression::Function(function) = &statement.value {
                functions.insert(statement.name.name.to_string(), function.clone());
            }
        }
        Statement::Expression(statement) => {
            if let Expression::Function(function) = &statement.expression {
                if let Some(name) = &function.name {
                    functions.insert(name.name.to_string(), function.clone());
                }
            }
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                collect_statement_named_functions(statement, functions, position);
            }
        }
        Statement::While(statement) => {
            if span_contains_position(statement.consequence.span, position) {
                for statement in &statement.consequence.statements {
                    collect_statement_named_functions(statement, functions, position);
                }
            }
        }
        Statement::ForIn(statement) => {
            if span_contains_position(statement.body.span, position) {
                for statement in &statement.body.statements {
                    collect_statement_named_functions(statement, functions, position);
                }
            }
        }
        _ => {}
    }
}

fn infer_function_return_shape(
    function: &FunctionLiteral,
    context: &mut ShapeContext,
) -> Option<ValueShape> {
    let mut variables = BTreeMap::new();
    let mut returns = Vec::new();
    let guaranteed = collect_return_shapes_from_statements(
        &function.body.statements,
        &mut variables,
        context,
        &mut returns,
        true,
    )?;
    if !guaranteed {
        return None;
    }
    unify_return_shapes(returns)
}

fn collect_return_shapes_from_statements(
    statements: &[Statement],
    variables: &mut BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
    returns: &mut Vec<ValueShape>,
    include_tail_expression: bool,
) -> Option<bool> {
    for (index, statement) in statements.iter().enumerate() {
        let is_tail = include_tail_expression && index + 1 == statements.len();
        if collect_return_shapes_from_statement(statement, variables, context, returns, is_tail)? {
            return Some(true);
        }
    }
    Some(false)
}

fn collect_return_shapes_from_statement(
    statement: &Statement,
    variables: &mut BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
    returns: &mut Vec<ValueShape>,
    is_tail: bool,
) -> Option<bool> {
    match statement {
        Statement::Let(statement) => {
            if let Some(shape) = expression_shape(&statement.value, variables, context) {
                variables.insert(statement.name.name.to_string(), shape);
            } else {
                variables.remove(&statement.name.name);
            }
            Some(false)
        }
        Statement::Assign(statement) => {
            if let AssignTarget::Identifier(identifier) = &statement.target {
                if let Some(shape) = expression_shape(&statement.value, variables, context) {
                    variables.insert(identifier.name.to_string(), shape);
                } else {
                    variables.remove(&identifier.name);
                }
            }
            Some(false)
        }
        Statement::Return(statement) => {
            returns.push(expression_shape(&statement.value, variables, context)?);
            Some(true)
        }
        Statement::Block(block) => collect_return_shapes_from_statements(
            &block.statements,
            variables,
            context,
            returns,
            false,
        ),
        Statement::While(statement) => {
            let mut nested_variables = variables.clone();
            collect_return_shapes_from_statements(
                &statement.consequence.statements,
                &mut nested_variables,
                context,
                returns,
                false,
            )?;
            Some(false)
        }
        Statement::ForIn(statement) => {
            let mut nested_variables = variables.clone();
            collect_return_shapes_from_statements(
                &statement.body.statements,
                &mut nested_variables,
                context,
                returns,
                false,
            )?;
            Some(false)
        }
        Statement::Expression(statement) => {
            if is_tail && !statement.end_of_semicolon {
                returns.push(tail_expression_shape(
                    &statement.expression,
                    variables,
                    context,
                )?);
                Some(true)
            } else {
                collect_return_shapes_from_expression(
                    &statement.expression,
                    variables,
                    context,
                    returns,
                )
            }
        }
        Statement::Empty(_)
        | Statement::Import(_)
        | Statement::Export(_)
        | Statement::Struct(_)
        | Statement::Break(_)
        | Statement::Continue(_) => Some(false),
    }
}

fn collect_return_shapes_from_expression(
    expression: &Expression,
    variables: &BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
    returns: &mut Vec<ValueShape>,
) -> Option<bool> {
    if let Expression::If(expression) = expression {
        let mut consequence_variables = variables.clone();
        let consequence_returns = collect_return_shapes_from_statements(
            &expression.consequence.statements,
            &mut consequence_variables,
            context,
            returns,
            false,
        )?;
        let alternative_returns = if let Some(alternative) = &expression.alternative {
            let mut alternative_variables = variables.clone();
            collect_return_shapes_from_statements(
                &alternative.statements,
                &mut alternative_variables,
                context,
                returns,
                false,
            )?
        } else if let Some(optional) = &expression.optional {
            collect_return_shapes_from_expression(optional, variables, context, returns)?
        } else {
            false
        };
        return Some(consequence_returns && alternative_returns);
    }
    Some(false)
}

fn tail_expression_shape(
    expression: &Expression,
    variables: &BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
) -> Option<ValueShape> {
    match expression {
        Expression::If(expression) => {
            let mut consequence_variables = variables.clone();
            let consequence_shape = block_tail_shape(
                &expression.consequence.statements,
                &mut consequence_variables,
                context,
            )?;
            let alternative_shape = if let Some(alternative) = &expression.alternative {
                let mut alternative_variables = variables.clone();
                block_tail_shape(&alternative.statements, &mut alternative_variables, context)?
            } else if let Some(optional) = &expression.optional {
                tail_expression_shape(optional, variables, context)?
            } else {
                return None;
            };
            if consequence_shape == alternative_shape {
                Some(consequence_shape)
            } else {
                None
            }
        }
        _ => expression_shape(expression, variables, context),
    }
}

fn block_tail_shape(
    statements: &[Statement],
    variables: &mut BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
) -> Option<ValueShape> {
    let mut returns = Vec::new();
    let guaranteed =
        collect_return_shapes_from_statements(statements, variables, context, &mut returns, true)?;
    if !guaranteed {
        return None;
    }
    unify_return_shapes(returns)
}

fn unify_return_shapes(returns: Vec<ValueShape>) -> Option<ValueShape> {
    let mut returns = returns.into_iter();
    let first = returns.next()?;
    if returns.all(|shape| shape == first) {
        Some(first)
    } else {
        None
    }
}

fn collect_variable_shapes(
    program: &Program,
    position: Option<SourcePosition>,
) -> BTreeMap<String, ValueShape> {
    let mut variables = BTreeMap::new();
    let mut context = ShapeContext::new(program, position);
    for statement in &program.statements {
        collect_statement_variable_shapes(statement, &mut variables, position, &mut context);
    }
    variables
}

fn collect_statement_variable_shapes(
    statement: &Statement,
    variables: &mut BTreeMap<String, ValueShape>,
    position: Option<SourcePosition>,
    context: &mut ShapeContext,
) {
    if statement_starts_after(statement, position) {
        return;
    }
    match statement {
        Statement::Let(statement) => {
            if let Some(shape) = expression_shape(&statement.value, variables, context) {
                variables.insert(statement.name.name.to_string(), shape);
            } else {
                variables.remove(&statement.name.name);
            }
        }
        Statement::Assign(statement) => {
            if let AssignTarget::Identifier(identifier) = &statement.target {
                if let Some(shape) = expression_shape(&statement.value, variables, context) {
                    variables.insert(identifier.name.to_string(), shape);
                } else {
                    variables.remove(&identifier.name);
                }
            }
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                collect_statement_variable_shapes(statement, variables, position, context);
            }
        }
        Statement::While(statement) => {
            if span_contains_position(statement.consequence.span, position) {
                for statement in &statement.consequence.statements {
                    collect_statement_variable_shapes(statement, variables, position, context);
                }
            }
        }
        Statement::ForIn(statement) => {
            if span_contains_position(statement.body.span, position) {
                for statement in &statement.body.statements {
                    collect_statement_variable_shapes(statement, variables, position, context);
                }
            }
        }
        _ => {}
    }
}

fn statement_starts_after(statement: &Statement, position: Option<SourcePosition>) -> bool {
    let Some(position) = position else {
        return false;
    };
    let start = statement.span().start;
    start.0 > position.line || (start.0 == position.line && start.1 > position.character)
}

fn span_contains_position(span: Span, position: Option<SourcePosition>) -> bool {
    let Some(position) = position else {
        return true;
    };
    let after_start = span.start.0 < position.line
        || (span.start.0 == position.line && span.start.1 <= position.character);
    let before_end = span.end.0 > position.line
        || (span.end.0 == position.line && span.end.1 >= position.character);
    after_start && before_end
}

fn expression_shape(
    expression: &Expression,
    variables: &BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
) -> Option<ValueShape> {
    match expression {
        Expression::Call(call) => call_shape(&call.function, context),
        Expression::Identifier(identifier) => variables.get(&identifier.name).cloned(),
        Expression::Member(member) => {
            let left_shape = expression_shape(&member.left, variables, context)?;
            left_shape.member_shape(&member.property.name, &BTreeMap::new())
        }
        Expression::StructLiteral(literal) => {
            struct_type_name(&literal.name).map(ValueShape::Struct)
        }
        Expression::Map(map) => map_literal_shape(map, variables, context),
        _ => None,
    }
}

fn map_literal_shape(
    map: &crate::ast::MapLiteral,
    variables: &BTreeMap<String, ValueShape>,
    context: &mut ShapeContext,
) -> Option<ValueShape> {
    let has_ok = map_literal_has_key(map, "ok");
    let has_value = map_literal_has_key(map, "value");
    let has_error = map_literal_has_key(map, "error");
    if !has_ok || !has_value || !has_error {
        return None;
    }
    let value_shape = map
        .kv_pair
        .iter()
        .find_map(|(key, value)| {
            (map_literal_key_name(key).as_deref() == Some("value"))
                .then(|| expression_shape(value, variables, context))
                .flatten()
        })
        .unwrap_or(ValueShape::Unknown);
    Some(ValueShape::Result(Box::new(value_shape)))
}

fn map_literal_has_key(map: &crate::ast::MapLiteral, expected: &str) -> bool {
    map.kv_pair
        .iter()
        .any(|(key, _)| map_literal_key_name(key).as_deref() == Some(expected))
}

fn map_literal_key_name(expression: &Expression) -> Option<String> {
    match expression {
        Expression::String(value) => Some(value.value.to_string()),
        _ => None,
    }
}

fn resolve_shape_path(
    path: &[String],
    variables: &BTreeMap<String, ValueShape>,
    struct_fields: &BTreeMap<String, Vec<String>>,
) -> Option<ValueShape> {
    let mut shape = variables.get(path.first()?)?.clone();
    for property in &path[1..] {
        shape = shape.member_shape(property, struct_fields)?;
    }
    Some(shape)
}

fn call_shape(function: &Expression, context: &mut ShapeContext) -> Option<ValueShape> {
    let path = expression_path(function)?;
    known_call_shape(&path).or_else(|| {
        if path.len() == 1 {
            context.function_return_shape(&path[0])
        } else {
            None
        }
    })
}

fn expression_path(expression: &Expression) -> Option<Vec<String>> {
    match expression {
        Expression::Identifier(identifier) => Some(vec![identifier.name.to_string()]),
        Expression::Member(member) => {
            let mut path = expression_path(&member.left)?;
            path.push(member.property.name.to_string());
            Some(path)
        }
        _ => None,
    }
}

fn known_call_shape(path: &[String]) -> Option<ValueShape> {
    let name = path.last()?.as_str();
    builtin_signature(name)?.return_shape
}

#[derive(Clone)]
struct BuiltinSignature {
    params: &'static str,
    return_type: Option<&'static str>,
    return_shape: Option<ValueShape>,
}

impl BuiltinSignature {
    fn detail(&self, name: &str) -> String {
        let signature = format!("fn {name}({})", self.params);
        match self.return_type {
            Some(return_type) => format!("{signature} -> {return_type}"),
            None => signature,
        }
    }

    fn builtin_detail(&self, name: &str) -> String {
        format!("builtin {}", self.detail(name))
    }
}

fn builtin_signature(name: &str) -> Option<BuiltinSignature> {
    let result = |value| Some(ValueShape::Result(Box::new(value)));
    Some(match name {
        "print" => BuiltinSignature {
            params: "values...",
            return_type: Some("null"),
            return_shape: None,
        },
        "len" | "byte_len" => BuiltinSignature {
            params: "value",
            return_type: Some("int"),
            return_shape: None,
        },
        "str" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "int" => BuiltinSignature {
            params: "value",
            return_type: Some("int"),
            return_shape: None,
        },
        "float" => BuiltinSignature {
            params: "value",
            return_type: Some("float"),
            return_shape: None,
        },
        "parse_int" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<int>"),
            return_shape: result(ValueShape::Unknown),
        },
        "parse_float" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<float>"),
            return_shape: result(ValueShape::Unknown),
        },
        "append" => BuiltinSignature {
            params: "list, value",
            return_type: Some("null"),
            return_shape: None,
        },
        "delete" => BuiltinSignature {
            params: "list_or_map, key",
            return_type: Some("value"),
            return_shape: None,
        },
        "join" => BuiltinSignature {
            params: "list, separator",
            return_type: Some("string"),
            return_shape: None,
        },
        "read_file" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<string>"),
            return_shape: result(ValueShape::Unknown),
        },
        "write_file" | "append_file" => BuiltinSignature {
            params: "path, content",
            return_type: Some("Result<int>"),
            return_shape: result(ValueShape::Unknown),
        },
        "file_exists" => BuiltinSignature {
            params: "path",
            return_type: Some("bool"),
            return_shape: None,
        },
        "read_dir" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<list>"),
            return_shape: result(ValueShape::Unknown),
        },
        "mkdir" | "remove_file" | "remove_dir" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<null>"),
            return_shape: result(ValueShape::Unknown),
        },
        "copy_file" => BuiltinSignature {
            params: "from, to",
            return_type: Some("Result<int>"),
            return_shape: result(ValueShape::Unknown),
        },
        "rename" => BuiltinSignature {
            params: "from, to",
            return_type: Some("Result<null>"),
            return_shape: result(ValueShape::Unknown),
        },
        "metadata" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<{exists, is_file, is_dir, size, modified_ms}>"),
            return_shape: result(ValueShape::Metadata),
        },
        "path_join" => BuiltinSignature {
            params: "parts...",
            return_type: Some("string"),
            return_shape: None,
        },
        "path_dirname" | "path_basename" | "path_ext" => BuiltinSignature {
            params: "path",
            return_type: Some("string"),
            return_shape: None,
        },
        "path_exists" | "path_is_file" | "path_is_dir" => BuiltinSignature {
            params: "path",
            return_type: Some("bool"),
            return_shape: None,
        },
        "env_get" => BuiltinSignature {
            params: "name",
            return_type: Some("string"),
            return_shape: None,
        },
        "env_set" => BuiltinSignature {
            params: "name, value",
            return_type: Some("null"),
            return_shape: None,
        },
        "cwd" => BuiltinSignature {
            params: "",
            return_type: Some("Result<string>"),
            return_shape: result(ValueShape::Unknown),
        },
        "set_cwd" => BuiltinSignature {
            params: "path",
            return_type: Some("Result<null>"),
            return_shape: result(ValueShape::Unknown),
        },
        "exit" => BuiltinSignature {
            params: "code",
            return_type: Some("never"),
            return_shape: None,
        },
        "time_ms" | "now_ms" => BuiltinSignature {
            params: "",
            return_type: Some("int"),
            return_shape: None,
        },
        "sleep_ms" => BuiltinSignature {
            params: "ms",
            return_type: Some("null"),
            return_shape: None,
        },
        "sleep" => BuiltinSignature {
            params: "seconds",
            return_type: Some("null"),
            return_shape: None,
        },
        "clear" | "home" | "hide_cursor" | "show_cursor" | "enable_raw_mode"
        | "disable_raw_mode" | "clear_line" | "enter_alt_screen" | "leave_alt_screen" | "bold"
        | "reset_style" => BuiltinSignature {
            params: "",
            return_type: Some("null"),
            return_shape: None,
        },
        "fg" | "bg" => BuiltinSignature {
            params: "color",
            return_type: Some("null"),
            return_shape: None,
        },
        "paint" => BuiltinSignature {
            params: "color, text",
            return_type: Some("string"),
            return_shape: None,
        },
        "paint_runs" => BuiltinSignature {
            params: "runs",
            return_type: Some("string"),
            return_shape: None,
        },
        "read_key" => BuiltinSignature {
            params: "",
            return_type: Some("Result<{kind, key, ctrl, alt, shift}>"),
            return_shape: result(ValueShape::TerminalKey),
        },
        "read_key_timeout" => BuiltinSignature {
            params: "ms",
            return_type: Some("Result<{kind, key, ctrl, alt, shift}|null>"),
            return_shape: result(ValueShape::TerminalKey),
        },
        "read_key_latest_timeout" => BuiltinSignature {
            params: "ms",
            return_type: Some("Result<{kind, key, ctrl, alt, shift}|null>"),
            return_shape: result(ValueShape::TerminalKey),
        },
        "move" => BuiltinSignature {
            params: "row, col",
            return_type: Some("null"),
            return_shape: None,
        },
        "size" => BuiltinSignature {
            params: "",
            return_type: Some("Result<{cols, rows}>"),
            return_shape: result(ValueShape::TerminalSize),
        },
        "abs" | "floor" | "ceil" | "round" | "sqrt" => BuiltinSignature {
            params: "value",
            return_type: Some("number"),
            return_shape: None,
        },
        "pow" => BuiltinSignature {
            params: "base, exponent",
            return_type: Some("number"),
            return_shape: None,
        },
        "min" | "max" => BuiltinSignature {
            params: "values...",
            return_type: Some("number"),
            return_shape: None,
        },
        "random_int" => BuiltinSignature {
            params: "min, max",
            return_type: Some("int"),
            return_shape: None,
        },
        "random_float" => BuiltinSignature {
            params: "",
            return_type: Some("float"),
            return_shape: None,
        },
        "sort" => BuiltinSignature {
            params: "list",
            return_type: Some("list"),
            return_shape: None,
        },
        "http_get" => BuiltinSignature {
            params: "url, headers?",
            return_type: Some("Result<{status, status_ok, headers, body}>"),
            return_shape: result(ValueShape::HttpResponse),
        },
        "http_post" => BuiltinSignature {
            params: "url, body, headers?",
            return_type: Some("Result<{status, status_ok, headers, body}>"),
            return_shape: result(ValueShape::HttpResponse),
        },
        "http_request" => BuiltinSignature {
            params: "method, url, body, headers?",
            return_type: Some("Result<{status, status_ok, headers, body}>"),
            return_shape: result(ValueShape::HttpResponse),
        },
        "exec" => BuiltinSignature {
            params: "command, args?",
            return_type: Some("Result<{success, status, stdout, stderr}>"),
            return_shape: result(ValueShape::ExecResult),
        },
        "url_encode" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "url_decode" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<string>"),
            return_shape: result(ValueShape::Unknown),
        },
        "base64_encode" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "base64_decode" => BuiltinSignature {
            params: "value",
            return_type: Some("Result<string>"),
            return_shape: result(ValueShape::Unknown),
        },
        "sha256" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "json_stringify" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "args" => BuiltinSignature {
            params: "",
            return_type: Some("list"),
            return_shape: None,
        },
        "read_line" => BuiltinSignature {
            params: "",
            return_type: Some("Result<string>"),
            return_shape: result(ValueShape::Unknown),
        },
        "panic" => BuiltinSignature {
            params: "message",
            return_type: Some("never"),
            return_shape: None,
        },
        "assert" => BuiltinSignature {
            params: "condition, message?",
            return_type: Some("null"),
            return_shape: None,
        },
        "substr" => BuiltinSignature {
            params: "text, start, length?",
            return_type: Some("string"),
            return_shape: None,
        },
        "find" => BuiltinSignature {
            params: "text, pattern",
            return_type: Some("int"),
            return_shape: None,
        },
        "replace" => BuiltinSignature {
            params: "text, from, to",
            return_type: Some("string"),
            return_shape: None,
        },
        "char_code" => BuiltinSignature {
            params: "text",
            return_type: Some("int"),
            return_shape: None,
        },
        "from_char_code" => BuiltinSignature {
            params: "code",
            return_type: Some("string"),
            return_shape: None,
        },
        "type" => BuiltinSignature {
            params: "value",
            return_type: Some("string"),
            return_shape: None,
        },
        "is_null" | "is_bool" | "is_int" | "is_float" | "is_string" | "is_list" | "is_map" => {
            BuiltinSignature {
                params: "value",
                return_type: Some("bool"),
                return_shape: None,
            }
        }
        _ => return None,
    })
}

fn collect_visible_struct_fields(
    program: &Program,
    current_path: Option<&str>,
    position: Option<SourcePosition>,
) -> BTreeMap<String, Vec<String>> {
    let mut fields = BTreeMap::new();
    for (alias, import_path) in imports(program, position) {
        let imported = imported_struct_fields(current_path, import_path.as_str());
        match alias {
            Some(alias) => {
                for (name, struct_fields) in imported {
                    fields.insert(format!("{alias}.{name}"), struct_fields);
                }
            }
            None => fields.extend(imported),
        }
    }
    fields.extend(collect_struct_fields(program, position));
    fields
}

fn imports(program: &Program, position: Option<SourcePosition>) -> Vec<(Option<String>, String)> {
    program
        .statements
        .iter()
        .filter(|statement| !statement_starts_after(statement, position))
        .filter_map(|statement| match statement {
            Statement::Import(statement) => Some((
                statement.alias.as_ref().map(|alias| alias.name.to_string()),
                statement.path.value.to_string(),
            )),
            _ => None,
        })
        .collect()
}

fn imported_struct_fields(
    current_path: Option<&str>,
    import_path: &str,
) -> BTreeMap<String, Vec<String>> {
    let Some(path) = resolve_import_path(current_path, import_path) else {
        return BTreeMap::new();
    };
    let Ok(source) = fs::read_to_string(path.as_path()) else {
        return BTreeMap::new();
    };
    let Ok(program) = parse_program(&source, Some(path.to_string_lossy().into_owned())) else {
        return BTreeMap::new();
    };
    let exported = program
        .statements
        .iter()
        .filter_map(|statement| match statement {
            Statement::Export(statement) => Some(statement.name.name.to_string()),
            _ => None,
        })
        .collect::<Vec<_>>();
    collect_struct_fields(&program, None)
        .into_iter()
        .filter(|(name, _)| exported.iter().any(|export| export == name))
        .collect()
}

fn module_member_completions(
    program: &Program,
    path: Option<&str>,
    receiver: &str,
    position: SourcePosition,
) -> Option<Vec<Symbol>> {
    let import_path = program
        .statements
        .iter()
        .filter(|statement| !statement_starts_after(statement, Some(position)))
        .find_map(|statement| match statement {
            Statement::Import(statement) => statement
                .alias
                .as_ref()
                .filter(|alias| alias.name == receiver)
                .map(|_| statement.path.value.to_string()),
            _ => None,
        })?;
    Some(module_exports(path, import_path.as_str()))
}

fn module_exports(current_path: Option<&str>, import_path: &str) -> Vec<Symbol> {
    let Some(path) = resolve_import_path(current_path, import_path) else {
        return Vec::new();
    };
    let Ok(source) = fs::read_to_string(path.as_path()) else {
        return Vec::new();
    };
    let Ok(program) = parse_program(&source, Some(path.to_string_lossy().into_owned())) else {
        return Vec::new();
    };
    let declarations = collect_declaration_symbols(&program);
    let builtins = new_builtin_function_map();
    program
        .statements
        .iter()
        .filter_map(|statement| match statement {
            Statement::Export(statement) => Some(statement.name.name.as_str()),
            _ => None,
        })
        .map(|name| {
            if let Some(declaration) = declarations.get(name) {
                declaration.clone()
            } else {
                let builtin_signature = builtins
                    .contains_key(name)
                    .then(|| builtin_signature(name))
                    .flatten();
                let kind = if builtin_signature.is_some() {
                    SymbolKind::Function
                } else {
                    SymbolKind::Field
                };
                Symbol {
                    label: name.to_string(),
                    kind,
                    detail: builtin_signature
                        .map(|signature| signature.detail(name))
                        .unwrap_or_else(|| format!("export {name}")),
                }
            }
        })
        .collect()
}

fn resolve_import_path(current_path: Option<&str>, import_path: &str) -> Option<PathBuf> {
    let path = Path::new(import_path);
    if path.is_absolute() {
        return Some(path.to_path_buf());
    }
    if let Some(current_path) = current_path {
        let relative = Path::new(current_path).parent()?.join(import_path);
        if relative.exists() {
            return Some(relative);
        }
    }
    let cwd_relative = std::env::current_dir().ok()?.join(import_path);
    if cwd_relative.exists() {
        Some(cwd_relative)
    } else {
        None
    }
}

fn collect_declaration_symbols(program: &Program) -> BTreeMap<String, Symbol> {
    let mut declarations = BTreeMap::new();
    let mut context = ShapeContext::new(program, None);
    for statement in &program.statements {
        match statement {
            Statement::Struct(statement) => {
                declarations.insert(
                    statement.name.name.to_string(),
                    Symbol {
                        label: statement.name.name.to_string(),
                        kind: SymbolKind::Struct,
                        detail: format!("struct {}", statement.name.name),
                    },
                );
            }
            Statement::Expression(statement) => {
                if let Expression::Function(function) = &statement.expression {
                    if let Some(name) = &function.name {
                        declarations.insert(
                            name.name.to_string(),
                            Symbol {
                                label: name.name.to_string(),
                                kind: SymbolKind::Function,
                                detail: function_signature(function, Some(&mut context)),
                            },
                        );
                    }
                }
            }
            Statement::Let(statement) => {
                declarations.insert(
                    statement.name.name.to_string(),
                    Symbol {
                        label: statement.name.name.to_string(),
                        kind: SymbolKind::Variable,
                        detail: format!("let {}", statement.name.name),
                    },
                );
            }
            _ => {}
        }
    }
    declarations
}

fn collect_struct_fields(
    program: &Program,
    position: Option<SourcePosition>,
) -> BTreeMap<String, Vec<String>> {
    let mut fields = BTreeMap::new();
    for statement in &program.statements {
        collect_statement_struct_fields(statement, &mut fields, position);
    }
    fields
}

fn collect_statement_struct_fields(
    statement: &Statement,
    fields: &mut BTreeMap<String, Vec<String>>,
    position: Option<SourcePosition>,
) {
    if statement_starts_after(statement, position) {
        return;
    }
    match statement {
        Statement::Struct(statement) => {
            fields.insert(
                statement.name.name.to_string(),
                statement
                    .fields
                    .iter()
                    .map(|field| field.name.to_string())
                    .collect(),
            );
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                collect_statement_struct_fields(statement, fields, position);
            }
        }
        Statement::While(statement) => {
            if span_contains_position(statement.consequence.span, position) {
                for statement in &statement.consequence.statements {
                    collect_statement_struct_fields(statement, fields, position);
                }
            }
        }
        Statement::ForIn(statement) => {
            if span_contains_position(statement.body.span, position) {
                for statement in &statement.body.statements {
                    collect_statement_struct_fields(statement, fields, position);
                }
            }
        }
        _ => {}
    }
}

fn collect_variable_struct_types(
    program: &Program,
    position: Option<SourcePosition>,
) -> BTreeMap<String, String> {
    let mut variables = BTreeMap::new();
    for statement in &program.statements {
        collect_statement_variable_struct_types(statement, &mut variables, position);
    }
    variables
}

fn collect_statement_variable_struct_types(
    statement: &Statement,
    variables: &mut BTreeMap<String, String>,
    position: Option<SourcePosition>,
) {
    if statement_starts_after(statement, position) {
        return;
    }
    match statement {
        Statement::Let(statement) => {
            if let Some(struct_name) = struct_literal_type(&statement.value) {
                variables.insert(statement.name.name.to_string(), struct_name);
            }
        }
        Statement::Assign(statement) => {
            if let AssignTarget::Identifier(identifier) = &statement.target {
                if let Some(struct_name) = struct_literal_type(&statement.value) {
                    variables.insert(identifier.name.to_string(), struct_name);
                }
            }
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                collect_statement_variable_struct_types(statement, variables, position);
            }
        }
        Statement::While(statement) => {
            if span_contains_position(statement.consequence.span, position) {
                for statement in &statement.consequence.statements {
                    collect_statement_variable_struct_types(statement, variables, position);
                }
            }
        }
        Statement::ForIn(statement) => {
            if span_contains_position(statement.body.span, position) {
                for statement in &statement.body.statements {
                    collect_statement_variable_struct_types(statement, variables, position);
                }
            }
        }
        _ => {}
    }
}

fn struct_literal_type(expression: &Expression) -> Option<String> {
    match expression {
        Expression::StructLiteral(literal) => struct_type_name(&literal.name),
        _ => None,
    }
}

fn struct_type_name(expression: &Expression) -> Option<String> {
    match expression {
        Expression::Identifier(identifier) => Some(identifier.name.to_string()),
        Expression::Member(member) => Some(format!(
            "{}.{}",
            struct_type_name(&member.left)?,
            member.property.name
        )),
        _ => None,
    }
}

fn diagnostic_json(err: &Error) -> String {
    let (start_line, start_character, end_line, end_character) = match err.span {
        Some(span) => lsp_range(span),
        None => (0, 0, 0, 1),
    };
    format!(
        "{{\"message\":\"{}\",\"severity\":1,\"source\":\"monkey\",\"range\":{{\"start\":{{\"line\":{},\"character\":{}}},\"end\":{{\"line\":{},\"character\":{}}}}}}}",
        escape_json(&err.msg),
        start_line,
        start_character,
        end_line,
        end_character
    )
}

fn lsp_range(span: Span) -> (usize, usize, usize, usize) {
    (
        span.start.0.saturating_sub(1),
        span.start.1.saturating_sub(1),
        span.end.0.saturating_sub(1),
        span.end.1.saturating_sub(1).max(span.start.1),
    )
}

#[derive(Clone)]
struct Symbol {
    label: String,
    kind: SymbolKind,
    detail: String,
}

#[derive(Clone)]
enum SymbolKind {
    Function,
    Variable,
    Struct,
    Module,
    Builtin,
    Keyword,
    Field,
    File,
    Folder,
}

impl SymbolKind {
    fn as_str(&self) -> &'static str {
        match self {
            SymbolKind::Function => "function",
            SymbolKind::Variable => "variable",
            SymbolKind::Struct => "struct",
            SymbolKind::Module => "module",
            SymbolKind::Builtin => "builtin",
            SymbolKind::Keyword => "keyword",
            SymbolKind::Field => "field",
            SymbolKind::File => "file",
            SymbolKind::Folder => "folder",
        }
    }
}

fn builtin_symbols() -> BTreeMap<String, Symbol> {
    new_global_builtin_function_map()
        .keys()
        .map(|name| {
            (
                (*name).to_string(),
                Symbol {
                    label: (*name).to_string(),
                    kind: SymbolKind::Builtin,
                    detail: builtin_signature(name)
                        .map(|signature| signature.builtin_detail(name))
                        .unwrap_or_else(|| format!("builtin fn {name}")),
                },
            )
        })
        .collect()
}

fn collect_keyword_symbols(symbols: &mut BTreeMap<String, Symbol>) {
    for keyword in [
        "let", "fn", "struct", "import", "from", "export", "if", "else", "while", "for", "in",
        "break", "continue", "return", "true", "false", "null",
    ] {
        insert_symbol(
            symbols,
            keyword,
            SymbolKind::Keyword,
            format!("keyword {keyword}"),
        );
    }
}

fn collect_program_symbols(program: &Program, symbols: &mut BTreeMap<String, Symbol>) {
    let mut context = ShapeContext::new(program, None);
    for statement in program.statements.iter() {
        collect_statement_symbols(statement, symbols, &mut context);
    }
}

fn collect_statement_symbols(
    statement: &Statement,
    symbols: &mut BTreeMap<String, Symbol>,
    context: &mut ShapeContext,
) {
    match statement {
        Statement::Let(statement) => {
            let (kind, detail) = match &statement.value {
                Expression::Function(function) => {
                    let signature = function_signature(function, Some(context));
                    (
                        SymbolKind::Function,
                        format!("let {} = {signature}", statement.name.name),
                    )
                }
                _ => (SymbolKind::Variable, format!("let {}", statement.name.name)),
            };
            insert_symbol(symbols, &statement.name.name, kind, detail);
            collect_expression_symbols(&statement.value, symbols, context);
        }
        Statement::Struct(statement) => {
            let fields = statement
                .fields
                .iter()
                .map(|field| field.name.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            insert_symbol(
                symbols,
                &statement.name.name,
                SymbolKind::Struct,
                format!("struct {} {{ {} }}", statement.name.name, fields),
            );
        }
        Statement::Import(statement) => {
            if let Some(alias) = &statement.alias {
                insert_symbol(
                    symbols,
                    &alias.name,
                    SymbolKind::Module,
                    format!("module from {}", statement.path.value),
                );
            }
        }
        Statement::Expression(statement) => {
            collect_expression_symbols(&statement.expression, symbols, context);
        }
        Statement::Block(block) => {
            for statement in block.statements.iter() {
                collect_statement_symbols(statement, symbols, context);
            }
        }
        Statement::While(statement) => {
            collect_expression_symbols(&statement.condition, symbols, context);
            for statement in statement.consequence.statements.iter() {
                collect_statement_symbols(statement, symbols, context);
            }
        }
        Statement::ForIn(statement) => {
            insert_symbol(
                symbols,
                &statement.key.name,
                SymbolKind::Variable,
                format!("for variable {}", statement.key.name),
            );
            if let Some(value) = &statement.value {
                insert_symbol(
                    symbols,
                    &value.name,
                    SymbolKind::Variable,
                    format!("for variable {}", value.name),
                );
            }
            collect_expression_symbols(&statement.collection, symbols, context);
            for statement in statement.body.statements.iter() {
                collect_statement_symbols(statement, symbols, context);
            }
        }
        Statement::Assign(statement) => {
            collect_expression_symbols(&statement.value, symbols, context);
        }
        Statement::Return(statement) => {
            collect_expression_symbols(&statement.value, symbols, context)
        }
        Statement::Empty(_)
        | Statement::Export(_)
        | Statement::Break(_)
        | Statement::Continue(_) => {}
    }
}

fn collect_expression_symbols(
    expression: &Expression,
    symbols: &mut BTreeMap<String, Symbol>,
    context: &mut ShapeContext,
) {
    match expression {
        Expression::Function(function) => {
            if let Some(name) = &function.name {
                insert_symbol(
                    symbols,
                    &name.name,
                    SymbolKind::Function,
                    function_signature(function, Some(context)),
                );
            }
            for parameter in function.parameters.iter() {
                insert_symbol(
                    symbols,
                    &parameter.name,
                    SymbolKind::Variable,
                    format!("parameter {}", parameter.name),
                );
            }
            for statement in function.body.statements.iter() {
                collect_statement_symbols(statement, symbols, context);
            }
        }
        Expression::Call(call) => {
            collect_expression_symbols(&call.function, symbols, context);
            for argument in call.arguments.iter() {
                collect_expression_symbols(argument, symbols, context);
            }
        }
        Expression::If(expression) => {
            collect_expression_symbols(&expression.condition, symbols, context);
            for statement in expression.consequence.statements.iter() {
                collect_statement_symbols(statement, symbols, context);
            }
            if let Some(alternative) = &expression.alternative {
                for statement in alternative.statements.iter() {
                    collect_statement_symbols(statement, symbols, context);
                }
            }
            if let Some(optional) = &expression.optional {
                collect_expression_symbols(optional, symbols, context);
            }
        }
        Expression::Infix(expression) => {
            collect_expression_symbols(&expression.left, symbols, context);
            collect_expression_symbols(&expression.right, symbols, context);
        }
        Expression::Prefix(expression) => {
            collect_expression_symbols(&expression.right, symbols, context)
        }
        Expression::Slice(slice) => {
            for element in slice.elements.iter() {
                collect_expression_symbols(element, symbols, context);
            }
        }
        Expression::Map(map) => {
            for (key, value) in map.kv_pair.iter() {
                collect_expression_symbols(key, symbols, context);
                collect_expression_symbols(value, symbols, context);
            }
        }
        Expression::StructLiteral(literal) => {
            collect_expression_symbols(&literal.name, symbols, context);
            for (_, value) in literal.fields.iter() {
                collect_expression_symbols(value, symbols, context);
            }
            for value in literal.values.iter() {
                collect_expression_symbols(value, symbols, context);
            }
        }
        Expression::Index(index) => {
            collect_expression_symbols(&index.left, symbols, context);
            collect_expression_symbols(&index.index, symbols, context);
        }
        Expression::Member(member) => collect_expression_symbols(&member.left, symbols, context),
        Expression::Bool(_)
        | Expression::Null(_)
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::String(_)
        | Expression::Identifier(_) => {}
    }
}

fn collect_text_symbols(source: &str, symbols: &mut BTreeMap<String, Symbol>) {
    for line in source.lines() {
        let line = line.split("//").next().unwrap_or("").trim_start();
        if let Some(name) = identifier_after_keyword(line, "let") {
            insert_symbol(symbols, name, SymbolKind::Variable, format!("let {name}"));
        } else if let Some(name) = identifier_after_keyword(line, "fn") {
            insert_symbol(symbols, name, SymbolKind::Function, format!("fn {name}"));
        } else if let Some(name) = identifier_after_keyword(line, "struct") {
            insert_symbol(symbols, name, SymbolKind::Struct, format!("struct {name}"));
        } else if let Some(alias) = import_alias(line) {
            insert_symbol(
                symbols,
                alias,
                SymbolKind::Module,
                format!("module {alias}"),
            );
        }
    }
}

fn identifier_after_keyword<'a>(line: &'a str, keyword: &str) -> Option<&'a str> {
    let rest = line.strip_prefix(keyword)?;
    let rest = rest.strip_prefix(char::is_whitespace)?.trim_start();
    read_identifier(rest)
}

fn import_alias(line: &str) -> Option<&str> {
    let rest = line.strip_prefix("import")?;
    let rest = rest.strip_prefix(char::is_whitespace)?.trim_start();
    let alias = read_identifier(rest)?;
    let after_alias = rest.get(alias.len()..)?.trim_start();
    if after_alias.starts_with("from") {
        Some(alias)
    } else {
        None
    }
}

fn read_identifier(input: &str) -> Option<&str> {
    let mut end = 0;
    for (index, ch) in input.char_indices() {
        if index == 0 {
            if ch != '_' && !ch.is_ascii_alphabetic() {
                return None;
            }
        } else if ch != '_' && !ch.is_ascii_alphanumeric() {
            break;
        }
        end = index + ch.len_utf8();
    }
    if end == 0 {
        None
    } else {
        Some(&input[..end])
    }
}

fn insert_symbol(
    symbols: &mut BTreeMap<String, Symbol>,
    label: &str,
    kind: SymbolKind,
    detail: String,
) {
    symbols.insert(
        label.to_string(),
        Symbol {
            label: label.to_string(),
            kind,
            detail,
        },
    );
}

fn function_signature(
    function: &crate::ast::FunctionLiteral,
    context: Option<&mut ShapeContext>,
) -> String {
    let params = function
        .parameters
        .iter()
        .map(|parameter| parameter.name.as_str())
        .collect::<Vec<_>>()
        .join(", ");
    let signature = match &function.name {
        Some(name) => format!("fn {}({params})", name.name),
        None => format!("fn({params})"),
    };
    match inferred_function_return_shape(function, context) {
        Some(shape) => format!("{signature} -> {}", shape.signature_name()),
        None => signature,
    }
}

fn inferred_function_return_shape(
    function: &crate::ast::FunctionLiteral,
    context: Option<&mut ShapeContext>,
) -> Option<ValueShape> {
    if let Some(context) = context {
        if let Some(name) = &function.name {
            return context.function_return_shape(&name.name);
        }
        return infer_function_return_shape(function, context);
    }
    None
}

fn escape_json(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            ch if ch.is_control() => escaped.push_str(format!("\\u{:04x}", ch as u32).as_str()),
            ch => escaped.push(ch),
        }
    }
    escaped
}

#[cfg(test)]
mod tests {
    use super::{
        completion_json, definition_json, diagnostics_json, escape_json, format_source_for_editor,
        references_json, symbols_json,
    };
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn monkey_string_content(value: &str) -> String {
        let mut output = String::new();
        for ch in value.chars() {
            match ch {
                '\\' => output.push_str("\\\\"),
                '"' => output.push_str("\\\""),
                '\n' => output.push_str("\\n"),
                '\r' => output.push_str("\\r"),
                '\t' => output.push_str("\\t"),
                '\0' => output.push_str("\\0"),
                _ => output.push(ch),
            }
        }
        output
    }

    fn monkey_string_literal(value: &str) -> String {
        format!("\"{}\"", monkey_string_content(value))
    }

    fn json_string_content(value: &str) -> String {
        escape_json(value)
    }

    #[test]
    fn diagnostics_reports_parse_errors_as_json() {
        let json = diagnostics_json("let x = ;", Some("test.monkey".to_string()));
        assert!(json.contains("\"diagnostics\":[{"));
        assert!(json.contains("\"message\":"));
    }

    #[test]
    fn editor_formatting_uses_pretty_formatter() {
        let formatted = format_source_for_editor("fn f(){let x=1;x}", None).unwrap();
        assert_eq!(formatted, "fn f() {\n    let x = 1;\n    x\n}");
    }

    #[test]
    fn symbols_include_builtins_and_user_bindings() {
        let json = symbols_json(
            r#"
            fn add(x, y) { x + y }
            let value = 1;
            struct User { name; }
            import math from "math.monkey";
            "#,
            None,
        );
        assert!(json.contains("\"label\":\"print\""));
        assert!(json.contains("\"label\":\"parse_int\""));
        assert!(json.contains("Result&lt;int&gt;") || json.contains("Result<int>"));
        assert!(!json.contains("\"label\":\"exec\""));
        assert!(!json.contains("\"label\":\"read_file\""));
        assert!(json.contains("\"label\":\"add\""));
        assert!(json.contains("\"label\":\"value\""));
        assert!(json.contains("\"label\":\"User\""));
        assert!(json.contains("\"label\":\"math\""));
        assert!(json.contains("\"label\":\"let\""));
        assert!(json.contains("\"label\":\"x\""));
        assert!(json.contains("\"label\":\"y\""));
    }

    #[test]
    fn symbols_are_best_effort_when_parse_fails() {
        let json = symbols_json(
            r#"
            let value = ;
            fn add(x, y) {
            struct User { name;
            import math from "math.monkey";
            "#,
            None,
        );
        assert!(json.contains("\"label\":\"print\""));
        assert!(json.contains("\"label\":\"value\""));
        assert!(json.contains("\"label\":\"add\""));
        assert!(json.contains("\"label\":\"User\""));
        assert!(json.contains("\"label\":\"math\""));
    }

    #[test]
    fn completion_returns_struct_fields_from_incomplete_member_expression() {
        let source = r#"
            struct User { name; age; }
            let user = User{name: "tom", age: 18};
            user.
            "#;
        let json = completion_json(source, Some("test.monkey".to_string()), 3, 17);
        assert!(json.contains("\"label\":\"name\""), "{json}");
        assert!(json.contains("\"label\":\"age\""), "{json}");
        assert!(json.contains("field User.name"), "{json}");
    }

    #[test]
    fn completion_returns_struct_fields_for_multiline_struct_with_slice_field() {
        let marked = r#"struct User {
    name;
    age;
    likes;
}
let user = User{name: "tom", age: 15, likes: ["apple", "banana"]};
user.|"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"name\""), "{json}");
        assert!(json.contains("\"label\":\"age\""), "{json}");
        assert!(json.contains("\"label\":\"likes\""), "{json}");
    }

    #[test]
    fn completion_returns_struct_fields_when_later_source_is_incomplete() {
        let marked = r#"struct User {
    name;
    age;
    likes;
}
let user = User{name: "tom", age: 15, likes: ["apple", "banana"]};
user.|
let broken = ;
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"name\""), "{json}");
        assert!(json.contains("\"label\":\"age\""), "{json}");
        assert!(json.contains("\"label\":\"likes\""), "{json}");
    }

    #[test]
    fn completion_returns_bare_imported_struct_fields() {
        let module_path = temp_file_path("monkey_completion_bare_struct.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        fs::write(
            module_path.as_str(),
            r#"
            struct User { name; age; }
            export User;
            "#,
        )
        .unwrap();
        let marked = format!(
            r#"
            import {module_literal};
            let user = User{{name: "tom", age: 18}};
            user.|
            "#
        );
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"name\""), "{json}");
        assert!(json.contains("\"label\":\"age\""), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn completion_prefers_local_struct_over_bare_imported_struct() {
        let module_path = temp_file_path("monkey_completion_shadowed_struct.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        fs::write(
            module_path.as_str(),
            r#"
            struct User { imported; }
            export User;
            "#,
        )
        .unwrap();
        let marked = format!(
            r#"
            import {module_literal};
            struct User {{ local; }}
            let user = User{{local: 1}};
            user.|
            "#
        );
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"local\""), "{json}");
        assert!(!json.contains("\"label\":\"imported\""), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn completion_returns_standard_result_fields() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.read_file("missing.txt");
            result.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"ok\""), "{json}");
        assert!(json.contains("\"label\":\"value\""), "{json}");
        assert!(json.contains("\"label\":\"error\""), "{json}");
    }

    #[test]
    fn completion_returns_standard_error_fields() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.read_file("missing.txt");
            result.error.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"kind\""), "{json}");
        assert!(json.contains("\"label\":\"code\""), "{json}");
        assert!(json.contains("\"label\":\"message\""), "{json}");
    }

    #[test]
    fn completion_propagates_result_value_shape_through_let() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.metadata("a.txt");
            let metadata = result.value;
            metadata.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
        assert!(json.contains("\"label\":\"is_dir\""), "{json}");
        assert!(json.contains("\"label\":\"size\""), "{json}");
        assert!(json.contains("\"label\":\"modified_ms\""), "{json}");
    }

    #[test]
    fn completion_propagates_result_error_shape_through_assignment() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.read_file("missing.txt");
            let err = null;
            err = result.error;
            err.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"kind\""), "{json}");
        assert!(json.contains("\"label\":\"code\""), "{json}");
        assert!(json.contains("\"label\":\"message\""), "{json}");
    }

    #[test]
    fn completion_propagates_result_shape_through_identifier_alias() {
        let marked = r#"
            import process from "stdlib/process.monkey";
            let result = process.exec("program", []);
            let alias = result;
            alias.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"success\""), "{json}");
        assert!(json.contains("\"label\":\"status\""), "{json}");
        assert!(json.contains("\"label\":\"stdout\""), "{json}");
        assert!(json.contains("\"label\":\"stderr\""), "{json}");
    }

    #[test]
    fn completion_ignores_assignments_after_cursor() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.metadata("a.txt");
            result.value.|;
            result = fs.read_file("a.txt");
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
        assert!(!json.contains("\"label\":\"ok\""), "{json}");
    }

    #[test]
    fn completion_does_not_treat_loop_assignments_as_definite_after_loop() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.metadata("a.txt");
            while false {
                result = fs.read_file("a.txt");
            }
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_infers_user_function_return_shape() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn get_meta(path) {
                return fs.metadata(path);
            }
            let result = get_meta("a.txt");
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
        assert!(json.contains("\"label\":\"modified_ms\""), "{json}");
    }

    #[test]
    fn completion_infers_user_function_return_shape_from_local_flow() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn get_error() {
                let result = fs.read_file("missing.txt");
                return result.error;
            }
            let err = get_error();
            err.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"kind\""), "{json}");
        assert!(json.contains("\"label\":\"code\""), "{json}");
        assert!(json.contains("\"label\":\"message\""), "{json}");
    }

    #[test]
    fn completion_infers_user_function_tail_expression_shape() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn get_meta(path) {
                fs.metadata(path)
            }
            let result = get_meta("a.txt");
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
        assert!(json.contains("\"label\":\"modified_ms\""), "{json}");
    }

    #[test]
    fn completion_infers_user_function_tail_expression_from_local_flow() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn get_error() {
                let result = fs.read_file("missing.txt");
                result.error
            }
            let err = get_error();
            err.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"kind\""), "{json}");
        assert!(json.contains("\"label\":\"code\""), "{json}");
        assert!(json.contains("\"label\":\"message\""), "{json}");
    }

    #[test]
    fn completion_infers_let_bound_function_return_shape() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let get_meta = fn(path) {
                fs.metadata(path)
            };
            let result = get_meta("a.txt");
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
        assert!(json.contains("\"label\":\"modified_ms\""), "{json}");
    }

    #[test]
    fn completion_does_not_infer_semicolon_terminated_tail_expression() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn get_meta(path) {
                fs.metadata(path);
            }
            let result = get_meta("a.txt");
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_infers_tail_if_else_expression_when_shapes_match() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    fs.metadata("a.txt")
                } else {
                    fs.metadata("b.txt")
                }
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_does_not_infer_tail_if_without_else() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    fs.metadata("a.txt")
                }
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_does_not_infer_tail_if_return_without_else() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    return fs.metadata("a.txt");
                }
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_rejects_tail_if_mixed_return_and_tail_shapes() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    return fs.metadata("a.txt");
                } else {
                    fs.read_file("a.txt")
                }
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_does_not_infer_conditional_return_without_fallback() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    return fs.metadata("a.txt");
                }
                print("done");
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_rejects_conditional_return_shape_conflict_before_fallback() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    return fs.read_file("a.txt");
                }
                return fs.metadata("a.txt");
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_infers_conditional_return_with_matching_fallback() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    return fs.metadata("a.txt");
                }
                return fs.metadata("b.txt");
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_skips_user_function_with_conflicting_return_shapes() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            fn load(flag) {
                if flag {
                    return fs.metadata("a.txt");
                }
                return fs.read_file("a.txt");
            }
            let result = load(true);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_ignores_user_functions_after_cursor() {
        let marked = r#"
            let result = get_meta("a.txt");
            result.value.|;
            import fs from "stdlib/fs.monkey";
            fn get_meta(path) {
                return fs.metadata(path);
            }
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"exists\""), "{json}");
        assert!(!json.contains("\"label\":\"is_file\""), "{json}");
    }

    #[test]
    fn completion_returns_exec_result_value_fields() {
        let marked = r#"
            import process from "stdlib/process.monkey";
            let result = process.exec("program", []);
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"success\""), "{json}");
        assert!(json.contains("\"label\":\"status\""), "{json}");
        assert!(json.contains("\"label\":\"stdout\""), "{json}");
        assert!(json.contains("\"label\":\"stderr\""), "{json}");
    }

    #[test]
    fn completion_returns_http_response_value_fields() {
        let marked = r#"
            import http from "stdlib/http.monkey";
            let result = http.http_get("https://example.com");
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"status\""), "{json}");
        assert!(json.contains("\"label\":\"status_ok\""), "{json}");
        assert!(json.contains("\"label\":\"headers\""), "{json}");
        assert!(json.contains("\"label\":\"body\""), "{json}");
    }

    #[test]
    fn completion_returns_metadata_value_fields() {
        let marked = r#"
            import fs from "stdlib/fs.monkey";
            let result = fs.metadata("a.txt");
            result.value.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"exists\""), "{json}");
        assert!(json.contains("\"label\":\"is_file\""), "{json}");
        assert!(json.contains("\"label\":\"is_dir\""), "{json}");
        assert!(json.contains("\"label\":\"size\""), "{json}");
        assert!(json.contains("\"label\":\"modified_ms\""), "{json}");
    }

    #[test]
    fn completion_propagates_struct_shape_through_identifier_alias() {
        let marked = r#"
            struct User { name; age; }
            let user = User{name: "A", age: 1};
            let other = user;
            other.|
            "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"name\""), "{json}");
        assert!(json.contains("\"label\":\"age\""), "{json}");
    }

    #[test]
    fn completion_ignores_imports_after_cursor() {
        let module_path = temp_file_path("monkey_completion_future_import.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        fs::write(
            module_path.as_str(),
            r#"
            let value = 1;
            export value;
            "#,
        )
        .unwrap();
        let marked = format!(
            r#"
            models.|;
            import models from {module_literal};
            "#
        );
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(!json.contains("\"label\":\"value\""), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn completion_returns_namespace_imported_struct_fields() {
        let module_path = temp_file_path("monkey_completion_namespace_struct.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        fs::write(
            module_path.as_str(),
            r#"
            struct User { name; age; }
            export User;
            "#,
        )
        .unwrap();
        let marked = format!(
            r#"
            import model from {module_literal};
            let user = model.User{{name: "tom", age: 18}};
            user.|
            "#
        );
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"name\""), "{json}");
        assert!(json.contains("\"label\":\"age\""), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn completion_returns_module_exports_from_rust_lang_service() {
        let module_path = temp_file_path("monkey_completion_module.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        fs::write(
            module_path.as_str(),
            r#"
            fn make_user() { 1 }
            struct User { name; }
            export make_user;
            export User;
            "#,
        )
        .unwrap();
        let source = format!(
            r#"
            import model from {module_literal};
            model.
            "#
        );
        let json = completion_json(&source, Some("test.monkey".to_string()), 2, 18);
        assert!(json.contains("\"label\":\"make_user\""), "{json}");
        assert!(json.contains("\"label\":\"User\""), "{json}");
        assert!(json.contains("fn make_user"), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn completion_returns_stdlib_json_export_signatures() {
        let marked = r#"
        import json from "stdlib/json.monkey";
        json.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"label\":\"json_parse\""), "{json}");
        assert!(json.contains("\"kind\":\"function\""), "{json}");
        assert!(
            json.contains("fn json_parse(src) -&gt; Result&lt;value&gt;")
                || json.contains("fn json_parse(src) -> Result<value>"),
            "{json}"
        );
        assert!(json.contains("\"label\":\"json_stringify\""), "{json}");
        assert!(
            json.contains("fn json_stringify(value) -&gt; string")
                || json.contains("fn json_stringify(value) -> string"),
            "{json}"
        );
    }

    #[test]
    fn completion_infers_result_map_return_signature() {
        let marked = r#"
        fn read() {
            {ok: true, value: 1, error: null}
        }
        rea|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"label\":\"read\""), "{json}");
        assert!(
            json.contains("fn read() -&gt; Result&lt;value&gt;")
                || json.contains("fn read() -> Result<value>"),
            "{json}"
        );
    }

    #[test]
    fn completion_returns_stdlib_io_exports() {
        let marked = r#"
        import io from "stdlib/io.monkey";
        io.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"label\":\"read_lines\""), "{json}");
        assert!(
            json.contains("fn read_lines(path) -&gt; Result&lt;value&gt;")
                || json.contains("fn read_lines(path) -> Result<value>"),
            "{json}"
        );
    }

    #[test]
    fn completion_returns_stdlib_time_sleep_signature() {
        let marked = r#"
        import time from "stdlib/time.monkey";
        time.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"label\":\"sleep\""), "{json}");
        assert!(json.contains("fn sleep(seconds)"), "{json}");
        assert!(json.contains("\"label\":\"sleep_ms\""), "{json}");
    }

    #[test]
    fn completion_returns_stdlib_terminal_exports() {
        let marked = r#"
        import terminal from "stdlib/terminal.monkey";
        terminal.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"label\":\"clear\""), "{json}");
        assert!(json.contains("fn clear()"), "{json}");
        assert!(json.contains("\"label\":\"home\""), "{json}");
        assert!(json.contains("\"label\":\"hide_cursor\""), "{json}");
        assert!(json.contains("\"label\":\"show_cursor\""), "{json}");
        assert!(json.contains("\"label\":\"enable_raw_mode\""), "{json}");
        assert!(json.contains("\"label\":\"disable_raw_mode\""), "{json}");
        assert!(json.contains("\"label\":\"read_key\""), "{json}");
        assert!(json.contains("fn read_key()"), "{json}");
        assert!(json.contains("\"label\":\"read_key_timeout\""), "{json}");
        assert!(json.contains("fn read_key_timeout(ms)"), "{json}");
        assert!(
            json.contains("\"label\":\"read_key_latest_timeout\""),
            "{json}"
        );
        assert!(json.contains("fn read_key_latest_timeout(ms)"), "{json}");
        assert!(json.contains("\"label\":\"move\""), "{json}");
        assert!(json.contains("fn move(row, col)"), "{json}");
        assert!(json.contains("\"label\":\"clear_line\""), "{json}");
        assert!(json.contains("\"label\":\"size\""), "{json}");
        assert!(json.contains("\"label\":\"enter_alt_screen\""), "{json}");
        assert!(json.contains("\"label\":\"leave_alt_screen\""), "{json}");
        assert!(json.contains("\"label\":\"fg\""), "{json}");
        assert!(json.contains("fn fg(color)"), "{json}");
        assert!(json.contains("\"label\":\"bg\""), "{json}");
        assert!(json.contains("fn bg(color)"), "{json}");
        assert!(json.contains("\"label\":\"bold\""), "{json}");
        assert!(json.contains("fn bold()"), "{json}");
        assert!(json.contains("\"label\":\"reset_style\""), "{json}");
        assert!(json.contains("fn reset_style()"), "{json}");
        assert!(json.contains("\"label\":\"paint\""), "{json}");
        assert!(json.contains("fn paint(color, text)"), "{json}");
        assert!(json.contains("\"label\":\"paint_runs\""), "{json}");
        assert!(json.contains("fn paint_runs(runs)"), "{json}");
    }

    #[test]
    fn completion_returns_stdlib_terminal_result_fields() {
        let marked = r#"
        import terminal from "stdlib/terminal.monkey";
        let size = terminal.size();
        size.value.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"cols\""), "{json}");
        assert!(json.contains("\"label\":\"rows\""), "{json}");

        let marked = r#"
        import terminal from "stdlib/terminal.monkey";
        let key = terminal.read_key_timeout(0);
        key.value.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"kind\""), "{json}");
        assert!(json.contains("\"label\":\"key\""), "{json}");
        assert!(json.contains("\"label\":\"ctrl\""), "{json}");
        assert!(json.contains("\"label\":\"alt\""), "{json}");
        assert!(json.contains("\"label\":\"shift\""), "{json}");

        let marked = r#"
        import terminal from "stdlib/terminal.monkey";
        let key = terminal.read_key_latest_timeout(0);
        key.value.|
        "#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(&source, Some("test.monkey".to_string()), line, character);
        assert!(json.contains("\"label\":\"kind\""), "{json}");
        assert!(json.contains("\"label\":\"key\""), "{json}");
        assert!(json.contains("\"label\":\"ctrl\""), "{json}");
        assert!(json.contains("\"label\":\"alt\""), "{json}");
        assert!(json.contains("\"label\":\"shift\""), "{json}");
    }

    #[test]
    fn completion_returns_import_paths_from_rust_lang_service() {
        let dir = temp_dir_path("monkey_completion_import_paths");
        fs::create_dir_all(dir.as_str()).unwrap();
        let module_path = std::path::Path::new(dir.as_str()).join("models.monkey");
        fs::write(
            module_path.as_path(),
            r#"
            struct User { name; }
            export User;
            "#,
        )
        .unwrap();
        let current_path = std::path::Path::new(dir.as_str()).join("main.monkey");
        let marked = r#"import "mo|"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = completion_json(
            &source,
            Some(current_path.to_string_lossy().into_owned()),
            line,
            character,
        );
        assert!(json.contains("\"label\":\"models.monkey\""), "{json}");
        assert!(json.contains("exports User"), "{json}");
        let _ = fs::remove_file(module_path);
        let _ = fs::remove_dir(dir);
    }

    #[test]
    fn completion_falls_back_to_global_symbols_outside_member_context() {
        let json = completion_json("pri", Some("test.monkey".to_string()), 0, 3);
        assert!(json.contains("\"label\":\"print\""), "{json}");
        assert!(json.contains("\"label\":\"parse_int\""), "{json}");
    }

    #[test]
    fn definition_returns_local_function_location() {
        let marked = r#"fn add(x) {
    x
}
let y = ad|d(1);
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"definition\":{"), "{json}");
        assert!(json.contains("\"path\":null"), "{json}");
        assert!(json.contains("\"line\":0,\"character\":3"), "{json}");
    }

    #[test]
    fn definition_returns_receiver_location_for_member_reference() {
        let marked = r#"struct User { name; }
let user = User{name: "tom"};
us|er.name;
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("\"definition\":{"), "{json}");
        assert!(json.contains("\"path\":null"), "{json}");
        assert!(json.contains("\"line\":1,\"character\":4"), "{json}");
    }

    #[test]
    fn definition_returns_bare_imported_export_location() {
        let module_path = temp_file_path("monkey_definition_bare_module.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        let module_json = json_string_content(module_path.as_str());
        fs::write(
            module_path.as_str(),
            "fn make_user() { 1 }\nexport make_user;\n",
        )
        .unwrap();
        let marked = format!("import {module_literal};\nmake_|user();\n");
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(
            json.contains(&format!("\"path\":\"{module_json}\"")),
            "{json}"
        );
        assert!(json.contains("\"line\":0,\"character\":3"), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn definition_returns_namespace_imported_member_location() {
        let module_path = temp_file_path("monkey_definition_namespace_module.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        let module_json = json_string_content(module_path.as_str());
        fs::write(
            module_path.as_str(),
            "fn make_user() { 1 }\nstruct User { name; }\nexport make_user;\nexport User;\n",
        )
        .unwrap();
        let marked = format!("import model from {module_literal};\nmodel.make_|user();\n");
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(
            json.contains(&format!("\"path\":\"{module_json}\"")),
            "{json}"
        );
        assert!(json.contains("\"line\":0,\"character\":3"), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn definition_returns_namespace_module_location_when_cursor_is_on_receiver() {
        let module_path = temp_file_path("monkey_definition_namespace_receiver.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        let module_json = json_string_content(module_path.as_str());
        fs::write(
            module_path.as_str(),
            "struct User { name; }\nexport User;\n",
        )
        .unwrap();
        let marked = format!("import model from {module_literal};\nmo|del.User;\n");
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(
            json.contains(&format!("\"path\":\"{module_json}\"")),
            "{json}"
        );
        assert!(json.contains("\"line\":0,\"character\":0"), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn definition_uses_cursor_segment_inside_nested_member_path() {
        let module_path = temp_file_path("monkey_definition_nested_namespace.monkey");
        let module_literal = monkey_string_literal(module_path.as_str());
        let module_json = json_string_content(module_path.as_str());
        fs::write(
            module_path.as_str(),
            "struct User { name; }\nexport User;\n",
        )
        .unwrap();
        let marked = format!("import model from {module_literal};\nmodel.Us|er.name;\n");
        let (source, line, character) = source_with_cursor(marked.as_str());
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(
            json.contains(&format!("\"path\":\"{module_json}\"")),
            "{json}"
        );
        assert!(json.contains("\"line\":0,\"character\":7"), "{json}");
        let _ = fs::remove_file(module_path);
    }

    #[test]
    fn definition_returns_stdlib_export_for_exported_builtin() {
        let marked = r#"import json from "stdlib/json.monkey";
json.json_string|ify({});
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = definition_json(&source, Some("test.monkey".to_string()), line, character);

        assert!(json.contains("stdlib/json.monkey"), "{json}");
        assert!(json.contains("\"character\":7"), "{json}");
        assert!(json.contains("\"character\":21"), "{json}");
        assert!(!json.contains("src/builtin.rs"), "{json}");
    }

    #[test]
    fn references_return_local_function_declaration_and_calls() {
        let marked = r#"fn ad|d(x) {
    x
}
let a = add(1);
let b = add(2);
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = references_json(
            &source,
            Some("test.monkey".to_string()),
            line,
            character,
            true,
        );

        assert_eq!(reference_count(&json), 3, "{json}");
        assert!(json.contains("\"line\":0,\"character\":3"), "{json}");
        assert!(json.contains("\"line\":3,\"character\":8"), "{json}");
        assert!(json.contains("\"line\":4,\"character\":8"), "{json}");
    }

    #[test]
    fn references_can_exclude_local_declaration() {
        let marked = r#"let val|ue = 1;
let other = value + value;
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = references_json(
            &source,
            Some("test.monkey".to_string()),
            line,
            character,
            false,
        );

        assert_eq!(reference_count(&json), 2, "{json}");
        assert!(!json.contains("\"line\":0,\"character\":4"), "{json}");
        assert!(json.contains("\"line\":1,\"character\":12"), "{json}");
        assert!(json.contains("\"line\":1,\"character\":20"), "{json}");
    }

    #[test]
    fn references_match_member_paths_without_collecting_same_named_identifiers() {
        let marked = r#"import model from "models.monkey";
let User = 1;
model.Us|er;
let user = model.User{name: "tom"};
model.Other;
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = references_json(
            &source,
            Some("test.monkey".to_string()),
            line,
            character,
            true,
        );

        assert_eq!(reference_count(&json), 2, "{json}");
        assert!(!json.contains("\"line\":1,\"character\":4"), "{json}");
        assert!(json.contains("\"line\":2,\"character\":6"), "{json}");
        assert!(json.contains("\"line\":3,\"character\":17"), "{json}");
    }

    #[test]
    fn references_do_not_cross_shadowed_local_bindings() {
        let marked = r#"let value = 1;
{
    let value = 2;
    val|ue;
}
value;
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = references_json(
            &source,
            Some("test.monkey".to_string()),
            line,
            character,
            true,
        );

        assert_eq!(reference_count(&json), 2, "{json}");
        assert!(!json.contains("\"line\":0,\"character\":4"), "{json}");
        assert!(json.contains("\"line\":2,\"character\":8"), "{json}");
        assert!(json.contains("\"line\":3,\"character\":4"), "{json}");
        assert!(!json.contains("\"line\":5,\"character\":0"), "{json}");
    }

    #[test]
    fn references_can_exclude_import_alias_declaration() {
        let marked = r#"import model from "models.monkey";
mod|el.User;
model.make_user();
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = references_json(
            &source,
            Some("test.monkey".to_string()),
            line,
            character,
            false,
        );

        assert_eq!(reference_count(&json), 2, "{json}");
        assert!(!json.contains("\"line\":0,\"character\":7"), "{json}");
        assert!(json.contains("\"line\":1,\"character\":0"), "{json}");
        assert!(json.contains("\"line\":2,\"character\":0"), "{json}");
    }

    #[test]
    fn references_include_recursive_let_bound_function_body() {
        let marked = r#"let f = fn() {
    f();
};
f|();
"#;
        let (source, line, character) = source_with_cursor(marked);
        let json = references_json(
            &source,
            Some("test.monkey".to_string()),
            line,
            character,
            true,
        );

        assert_eq!(reference_count(&json), 3, "{json}");
        assert!(json.contains("\"line\":0,\"character\":4"), "{json}");
        assert!(json.contains("\"line\":1,\"character\":4"), "{json}");
        assert!(json.contains("\"line\":3,\"character\":0"), "{json}");
    }

    fn temp_file_path(name: &str) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir()
            .join(format!("{nanos}_{name}"))
            .to_string_lossy()
            .into_owned()
    }

    fn temp_dir_path(name: &str) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir()
            .join(format!("{nanos}_{name}"))
            .to_string_lossy()
            .into_owned()
    }

    fn source_with_cursor(marked: &str) -> (String, usize, usize) {
        let cursor = marked.find('|').expect("cursor marker");
        let source = marked.replacen('|', "", 1);
        let before = &marked[..cursor];
        let line = before.chars().filter(|ch| *ch == '\n').count();
        let line_start = before.rfind('\n').map(|index| index + 1).unwrap_or(0);
        let character = before[line_start..].encode_utf16().count();
        (source, line, character)
    }

    fn reference_count(json: &str) -> usize {
        json.matches("\"range\"").count()
    }
}
