#![feature(str_strip)]
#![feature(trait_alias)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate calm_io;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate log;

#[macro_use]
mod util;

mod lexer;
mod parser;

use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

use lexer::Lexer;
use parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use util::TermChars;

const HISTORY_FILE: &str = ".slang_history";

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
        "lex" => {
            print_lex(arg);
        }
        "lexfile" => match read_file(arg) {
            Ok(contents) => {
                print_lex(&contents);
            }
            Err(err) => println!("{:?}", err),
        },
        "help" => println!("Commands: .lex"),
        cmd => println!("Unrecognized command: `{}`", cmd),
    }
}

fn read_file(name: &str) -> io::Result<String> {
    let path = "./examples/".to_owned() + name;
    let path = Path::new(&path);
    let mut file = File::open(path)?;
    let mut s = String::new();
    file.read_to_string(&mut s)?;
    Ok(s)
}

fn print_lex(source: &str) {
    let lexer = Lexer::new(source.chars());
    let parser = Parser::new(lexer);
    parser.dump_tokens();
}

fn interpret(source: &str) {}

fn handle_line(line: &str) {
    if line.starts_with('.') {
        handle_command(line)
    } else {
        interpret(line)
    }
}

fn main() {
    let term_chars = TermChars::new();
    println!("after term_chars");
    let lexer = Lexer::new(term_chars);
    println!("after lexer");
    let parser = Parser::new(lexer);
    println!("after parser");
    parser.dump_tokens();

    /*
    let mut line_editor = Editor::<()>::new();
    line_editor.load_history(HISTORY_FILE).unwrap_or(());

    loop {
        let line = line_editor.readline("~ ");
        match line {
            Ok(line) => {
                line_editor.add_history_entry(&line);
                handle_line(&line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(_) => break,
        }
    }

    line_editor.save_history(HISTORY_FILE).unwrap();
     */
}
