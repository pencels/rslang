use crate::lexer::Lexer;

pub struct Parser<Chars: Iterator<Item = char>> {
    lexer: Lexer<Chars>,
}

impl<Chars: Iterator<Item = char>> Parser<Chars> {
    pub fn new(lexer: Lexer<Chars>) -> Parser<Chars> {
        Parser { lexer }
    }

    pub fn dump_tokens(self) {
        println!("in dump_tokens");
        for token in self.lexer {
            match token {
                Ok((_, token, _)) => println!("{:?}", token),
                Err(err) => println!("{:?}", err),
            }
        }
    }
}
