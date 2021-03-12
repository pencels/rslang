use owned_chars::{OwnedChars, OwnedCharsExt};
use rustyline::{error::ReadlineError, Editor};

const HISTORY_FILE: &str = ".slang_history";
const PROMPT: &str = "~ ";

pub struct TermChars {
    editor: Editor<()>,
    current_line: Option<OwnedChars>,
}

impl TermChars {
    pub fn new() -> TermChars {
        let mut editor = Editor::<()>::new();
        editor.load_history(HISTORY_FILE).unwrap_or(());

        let mut term_chars = TermChars {
            editor,
            current_line: None,
        };
        term_chars.bump_line();
        term_chars
    }

    fn bump_line(&mut self) {
        loop {
            match self.editor.readline(PROMPT) {
                Ok(mut line) => {
                    self.editor.add_history_entry(&line);
                    line.push('\n');
                    self.current_line = Some(line.into_chars());
                    return;
                }
                Err(ReadlineError::Interrupted) => {
                    println!("^C");
                }
                Err(ReadlineError::Eof) => {
                    println!("^D");
                    self.current_line = None;
                    break;
                }
                Err(_) => {
                    self.current_line = None;
                    break;
                }
            }
        }
    }
}

impl Iterator for TermChars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.current_line {
                Some(ref mut chars) => match chars.next() {
                    Some(c) => return Some(c),
                    None => self.bump_line(),
                },
                None => return None,
            }
        }
    }
}

impl Drop for TermChars {
    fn drop(&mut self) {
        self.editor.save_history(HISTORY_FILE).unwrap();
    }
}
