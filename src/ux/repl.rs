use codespan_derive::IntoDiagnostic;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use rustyline::{
    error::ReadlineError,
    validate::{ValidationContext, ValidationResult, Validator},
    Editor,
};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

use crate::{
    ctx::SlangContext,
    lexer::{token::Token, Lexer, OperatorTrie},
    parser::{ast::*, result::ParseResult, Parser},
};

const HISTORY_FILE: &str = ".slang_history";
const PROMPT: &str = "~ ";

fn handle_command(command: &str) {
    let command = &command[1..]; // Cut off the '.'

    // Split command and arg to command.
    let mid = command
        .find(|c: char| c.is_ascii_whitespace())
        .unwrap_or(command.len());
    let (command_name, arg) = command.split_at(mid);
    let arg = arg
        .strip_prefix(|c: char| c.is_ascii_whitespace())
        .unwrap_or("");

    match command_name {
        "lex" => print_tokens(arg),
        "help" => println!("Commands: .lex"),
        cmd => println!("Unrecognized command: `{}`", cmd),
    }
}

/// Prints tokens (or lex errors) for the given source, with span info.
fn print_tokens(source: &str) {
    let mut trie = OperatorTrie::new();
    let lexer = Lexer::new(0, source, &mut trie);
    for token in lexer {
        match token {
            Ok(Token { span, ty }) => println!("Span({}..{}) {:?}", span.start, span.end, ty),
            Err(err) => println!("{:?}", err),
        }
    }
}

/*
fn validate_parse(ctx: &mut SlangContext, source: &str) -> ValidationResult {
    let lexer = Lexer::new(0, source, ctx.trie);
    let mut parser = Parser::new(0, lexer, ctx.precedence);
    let bump = Bump::new();
    match parser.parse_next_defn(&bump) {
        Err(ParseError::Incomplete) => ValidationResult::Incomplete,
        Err(_) => ValidationResult::Valid(None),
        Ok(_) => ValidationResult::Valid(None),
    }
}
 */

fn interpret(ctx: &mut SlangContext, source: &str, repl_index: &mut usize) {
    let file_id = ctx
        .files
        .add(format!("<stdin-{}>", repl_index), source.to_owned());
    let lexer = Lexer::new(file_id, source, &mut ctx.trie);
    let mut parser = Parser::new(
        file_id,
        lexer,
        &mut ctx.poset,
        &mut ctx.prefix_actions,
        &mut ctx.postfix_actions,
        &mut ctx.pattern_actions,
    );

    loop {
        match exec_next_defn(&mut parser) {
            Ok(None) => break,
            Ok(_) => {}
            Err(err) => {
                let diagnostic = err.into_diagnostic();
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &ctx.files, &diagnostic).unwrap();
                break;
            }
        }
    }
}

fn exec_next_defn(parser: &mut Parser) -> ParseResult<Option<()>> {
    if let Some(defn) = parser.parse_next_defn()? {
        eval_defn(parser, &defn)?;
    } else {
        return Ok(None);
    }

    Ok(Some(()))
}

fn eval_defn(parser: &mut Parser, defn: &Defn) -> ParseResult<()> {
    let Defn { kind, .. } = defn;
    match kind {
        DefnKind::Operator {
            fixity,
            assoc,
            ops,
            constraints,
        } => {
            parser.add_operator(fixity, assoc, ops, constraints)?;
        }
        _ => println!("{:#?}", kind),
    }

    Ok(())
}

fn handle_line(ctx: &mut SlangContext, line: &str, repl_index: &mut usize) {
    if line.starts_with('.') {
        handle_command(line)
    } else {
        interpret(ctx, line, repl_index);
        *repl_index += 1;
    }
}

#[derive(Completer, Helper, Hinter, Highlighter)]
struct SlangValidator;

impl Validator for SlangValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        let input = ctx.input();

        if input.starts_with('.') {
            return Ok(ValidationResult::Valid(None));
        }

        // TODO: have slang context so that you can access the trie + precedence info...
        Ok(ValidationResult::Valid(None))
    }

    fn validate_while_typing(&self) -> bool {
        false
    }
}

pub fn repl() {
    let mut ctx = SlangContext::new();
    let mut editor = Editor::new();
    editor.set_helper(Some(SlangValidator));
    editor.load_history(HISTORY_FILE).unwrap_or(());

    let mut repl_index = 0usize;
    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(&line);
                handle_line(&mut ctx, &line, &mut repl_index);
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                eprintln!("{:?}", err);
                break;
            }
        }
    }

    editor.save_history(HISTORY_FILE).unwrap();
}
