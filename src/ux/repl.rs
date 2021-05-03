use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

use codespan_derive::IntoDiagnostic;
use codespan_reporting::files::Files;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use gesture::EvalError;
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
    runtime::{gesture, value::Value, Env, Runtime},
    util::{pretty::Pretty, Id},
};

const HISTORY_FILE: &str = ".slang_history";
const PROMPT: &str = "~ ";
const CMD_LEADING_CHAR: char = '#';

fn handle_command(command: &str, ctx: &mut SlangContext) {
    let command = &command[1..]; // Cut off the leading char

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
        "run" => match load(arg, ctx) {
            Err(err) => eprintln!("{}", err),
            _ => {}
        },
        "methods" => {
            let root_env = ctx.root_env;
            let env = ctx.runtime.heap.get_env(root_env);
            let methods = env.method_table.methods();
            println!("{} methods in scope:", methods.len());
            for (i, method) in methods.iter().enumerate() {
                let name = ctx.files.name(method.span.file_id).unwrap();
                let line = ctx
                    .files
                    .line_index(method.span.file_id, method.span.start)
                    .unwrap();
                println!("[{}] {} -- at {}:{}", i, method, name, line);
            }
        }
        "help" => println!("Commands: #lex #run #help"),
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

fn load(filename: &str, ctx: &mut SlangContext) -> io::Result<()> {
    let path = Path::new(filename);
    let mut file = File::open(path)?;
    let mut buf = String::new();

    file.read_to_string(&mut buf)?;
    interpret(ctx, &buf, filename);

    Ok(())
}

fn interpret(ctx: &mut SlangContext, source: &str, filename: &str) {
    let file_id = ctx.files.add(filename.to_owned(), source.to_owned());
    let lexer = Lexer::new(file_id, source, &mut ctx.trie);
    let mut parser = Parser::new(
        file_id,
        lexer,
        &mut ctx.poset,
        &mut ctx.prefix_actions,
        &mut ctx.postfix_actions,
        &mut ctx.prefix_pattern_actions,
        &mut ctx.postfix_pattern_actions,
    );

    loop {
        match exec_next_defn(&mut parser, &mut ctx.runtime, ctx.root_env) {
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

fn exec_next_defn(
    parser: &mut Parser,
    runtime: &mut Runtime,
    env_id: Id<Env>,
) -> ParseResult<Option<()>> {
    if let Some(defn) = parser.parse_next_defn()? {
        eval_defn(parser, &defn, runtime, env_id)?;
    } else {
        return Ok(None);
    }

    Ok(Some(()))
}

fn eval_defn(
    parser: &mut Parser,
    defn: &Defn,
    runtime: &mut Runtime,
    env_id: Id<Env>,
) -> ParseResult<()> {
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
        DefnKind::Expr(expr) => match gesture::unravel_eval(runtime, env_id, expr) {
            Ok(Value::Nothing) => {}
            Ok(v) => println!("{}", Pretty(v, runtime)),
            Err(EvalError::Msg(msg)) => println!("Error: {}", msg),
            Err(EvalError::Panic(msg)) => println!("Panicked: {}", msg),
        },
        _ => println!("{:#?}", kind),
    }

    Ok(())
}

fn handle_line(ctx: &mut SlangContext, line: &str, repl_index: &mut usize) {
    if line.starts_with(CMD_LEADING_CHAR) {
        handle_command(line, ctx);
    } else {
        interpret(ctx, line, &format!("<stdin-{}>", repl_index));
        *repl_index += 1;
    }
}

#[derive(Completer, Helper, Hinter, Highlighter)]
struct SlangValidator;

impl Validator for SlangValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        let input = ctx.input();

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

    load("lib/prelude.slang", &mut ctx).unwrap();

    let mut repl_index = 0usize;
    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(&line);
                handle_line(&mut ctx, &line, &mut repl_index);
                println!();
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
