mod operator_trie;
mod token;

use lookahead::{lookahead, Lookahead};

pub use self::operator_trie::OperatorTrie;
pub use crate::lexer::token::*;
use crate::util::{PResult, Span};

/// Mnemonic for the EOF end-of-file character.
const EOF: char = '\x00';

pub type SpanToken = (usize, Token, usize);

pub struct Lexer<Chars: Iterator<Item = char>> {
    stream: Lookahead<Chars>,

    /// The trie determining max-munch operator chunky lip smack goodness.
    operator_trie: OperatorTrie,
    /// Whether the lexer should use the trie to lex operators, or if it should blindly lex them.
    use_trie: bool,

    current_pos: usize,
    current_char: char,
    next_char: char,

    interp_parenthetical: Vec<usize>,
}

pub enum LexStringChar {
    Char(char),
    QuoteEnd,
    InterpolateBegin,
}

impl<Chars: Iterator<Item = char>> Lexer<Chars> {
    pub fn new(chars: Chars) -> Lexer<Chars> {
        let mut operator_trie = OperatorTrie::new();
        operator_trie.insert_operator("+");
        operator_trie.insert_operator("-");
        operator_trie.insert_operator("*");
        operator_trie.insert_operator("/");
        operator_trie.insert_operator("++");

        let mut lex = Lexer {
            stream: lookahead(chars),
            operator_trie,
            use_trie: true,
            current_pos: 0,
            current_char: EOF,
            next_char: EOF,
            interp_parenthetical: vec![],
        };

        lex.bump(1);
        lex.current_pos = 0;
        lex
    }

    fn spanned_lex(&mut self) -> PResult<SpanToken> {
        self.discard_whitespace_or_comments()?;

        let start = self.current_pos;
        let t = self.lex()?;
        let end = self.current_pos;

        Ok((start, t, end))
    }

    fn bump(&mut self, n: usize) {
        for _ in 0..n {
            self.current_char = self.stream.next().unwrap_or(EOF);
            self.next_char = self.stream.lookahead(0).map_or(EOF, |&c| c);
            self.current_pos += 1;
        }
    }

    fn discard_whitespace_or_comments(&mut self) -> PResult<()> {
        loop {
            // Consume any whitespace or comments before a real token
            match self.current_char {
                '-' => {
                    // May be a comment or an operator/numeric
                    if self.next_char == '-' {
                        self.scan_comment()?;
                    } else {
                        break; // Operator/numeric
                    }
                }
                ' ' | '\t' | '\r' => self.bump(1),
                _ => {
                    break;
                }
            }
        }

        Ok(())
    }

    fn scan_comment(&mut self) -> PResult<()> {
        match self.next_char {
            '-' => {
                // Read until end of line.
                self.bump(2); // Read --.
                while self.current_char != '\r'
                    && self.current_char != '\n'
                    && self.current_char != EOF
                {
                    self.bump(1);
                }
            }
            _ => unreachable!("ICE: Expected -- to begin scanned comment"),
        }

        Ok(())
    }

    /// Returns the next lookahead token.
    fn lex(&mut self) -> PResult<Token> {
        let c = self.current_char;

        if c == EOF {
            Ok(Token::Eof)
        } else if c == '"' {
            self.scan_string()
        } else if is_numeric(c) {
            self.scan_numeric_literal()
        } else if is_identifier_start(c) {
            self.scan_identifier_or_keyword()
        } else {
            match c {
                '\n' => {
                    self.bump(1);
                    Ok(Token::Newline)
                }
                '.' => {
                    if self.next_char == '.' {
                        self.bump(1);
                        if self.next_char == '.' {
                            self.bump(2);
                            Ok(Token::Ellipsis)
                        } else {
                            self.scan_custom_operator("..")
                        }
                    } else {
                        self.scan_custom_operator(".")
                    }
                }
                ',' => {
                    self.bump(1);
                    Ok(Token::Comma)
                }
                ':' => {
                    if self.next_char == ':' {
                        self.bump(2);
                        Ok(Token::ColonColon)
                    } else {
                        self.scan_atom()
                    }
                }
                ';' => {
                    self.bump(1);
                    Ok(Token::Semicolon)
                }
                '{' => {
                    self.bump(1);
                    Ok(Token::LCurly)
                }
                '}' => {
                    self.bump(1);
                    Ok(Token::RCurly)
                }
                '[' => {
                    self.bump(1);
                    Ok(Token::LSquare)
                }
                ']' => {
                    self.bump(1);
                    Ok(Token::RSquare)
                }
                '(' => {
                    self.bump(1);

                    if let Some(nest) = self.interp_parenthetical.last_mut() {
                        *nest += 1;
                    }

                    Ok(Token::LParen)
                }
                ')' => {
                    if let Some(0) = self.interp_parenthetical.last() {
                        self.scan_interp_continue()
                    } else {
                        if let Some(n) = self.interp_parenthetical.last_mut() {
                            *n -= 1;
                        }

                        self.bump(1);
                        Ok(Token::RParen)
                    }
                }
                '=' => {
                    self.bump(1);
                    Ok(Token::Eq)
                }
                '-' => {
                    if is_numeric(self.next_char) {
                        self.scan_numeric_literal()
                    } else if self.next_char == '>' {
                        self.bump(2);
                        Ok(Token::Arrow)
                    } else {
                        self.bump(1);
                        self.scan_custom_operator("-")
                    }
                }
                '+' => {
                    if is_numeric(self.next_char) {
                        self.scan_numeric_literal()
                    } else {
                        self.scan_custom_operator("+")
                    }
                }
                '\\' => {
                    self.bump(1);
                    Ok(Token::Backslash)
                }
                c => perror_at!(
                    Span::new(self.current_pos, self.current_pos + 1),
                    "Unknown symbol '{}'",
                    c
                ),
            }
        }
    }

    /// Scans a custom operator.
    fn scan_custom_operator(&mut self, leading: &str) -> PResult<Token> {
        if !self.use_trie {
            return self.scan_arbitrary_operator(leading.to_owned());
        }

        let mut operator = leading.to_owned();
        let mut node = self.operator_trie.find(leading);
        let mut last_end = node.and_then(|node| if node.end { Some(0) } else { None });
        let mut lookahead = 0usize;

        // Traverse the trie with each operator char.
        while node.is_some() {
            match self.stream.lookahead(lookahead) {
                Some(&c) => {
                    if is_operator_boundary(c) {
                        break;
                    }
                    node = node.and_then(|node| node.get_child(c));
                    last_end = node
                        .and_then(|node| if node.end { Some(lookahead + 1) } else { None })
                        .or(last_end);
                    lookahead += 1;
                }
                None => break,
            }
        }

        match last_end {
            Some(n) => {
                self.bump(1);
                for _ in 0..n {
                    operator.push(self.current_char);
                    self.bump(1);
                }

                Ok(Token::Op(operator))
            }
            _ => self.scan_arbitrary_operator(operator),
        }
    }

    fn scan_arbitrary_operator(&mut self, leading: String) -> PResult<Token> {
        let mut operator = leading;

        self.bump(1);
        while !is_operator_boundary(self.current_char) {
            operator.push(self.current_char);
            self.bump(1);
        }

        Ok(Token::Op(operator))
    }

    /// Scans an atom token.
    fn scan_atom(&mut self) -> PResult<Token> {
        self.bump(1); // Eat the leading colon.
        let mut atom = String::new();

        // An atom can have any character following it, up until whitespace, delimiter/sequencing
        // characters, or the start of another atom/string.
        loop {
            match self.current_char {
                ' ' | '\t' | '\r' | '\n' | EOF | '(' | ')' | '[' | ']' | '{' | '}' | '\\' | ','
                | ';' | ':' | '"' => break,
                _ => {
                    atom.push(self.current_char);
                    self.bump(1);
                }
            }
        }

        // No empty atoms :)
        if atom.len() == 0 {
            return perror_at!(
                Span::new(self.current_pos - 1, self.current_pos),
                "Expected characters after `:` for atom name, none found",
            );
        }

        Ok(Token::Atom(atom))
    }

    /// Scans a new parsed string token.
    fn scan_string(&mut self) -> PResult<Token> {
        self.bump(1); // Blindly consume the quote character
        let mut string = String::new();

        loop {
            match self.scan_string_char()? {
                LexStringChar::Char(c) => {
                    string.push(c);
                }
                LexStringChar::QuoteEnd => {
                    return Ok(Token::Str(string));
                }
                LexStringChar::InterpolateBegin => {
                    self.interp_parenthetical.push(0);
                    return Ok(Token::InterpolateBegin(string));
                }
            }
        }
    }

    fn scan_interp_continue(&mut self) -> PResult<Token> {
        self.bump(1); // Blindly consume the rparen character
        let mut string = String::new();

        loop {
            match self.scan_string_char()? {
                LexStringChar::Char(c) => {
                    string.push(c);
                }
                LexStringChar::QuoteEnd => {
                    self.interp_parenthetical.pop();
                    return Ok(Token::InterpolateEnd(string));
                }
                LexStringChar::InterpolateBegin => {
                    return Ok(Token::InterpolateContinue(string));
                }
            }
        }
    }

    fn scan_string_char(&mut self) -> PResult<LexStringChar> {
        let ret;

        match self.current_char {
            '\\' => {
                match self.next_char {
                    'r' => {
                        ret = LexStringChar::Char('\r');
                    }
                    'n' => {
                        ret = LexStringChar::Char('\n');
                    }
                    't' => {
                        ret = LexStringChar::Char('\t');
                    }
                    '"' => {
                        ret = LexStringChar::Char('\"');
                    }
                    '\'' => {
                        ret = LexStringChar::Char('\'');
                    }
                    '\\' => {
                        ret = LexStringChar::Char('\\');
                    }
                    '(' => {
                        ret = LexStringChar::InterpolateBegin;
                    }
                    c => {
                        return perror_at!(
                            Span::new(self.current_pos, self.current_pos + 1),
                            "Unknown escaped character in string '\\{}'",
                            c
                        );
                    }
                }
                self.bump(2);
            }
            '\"' => {
                ret = LexStringChar::QuoteEnd;
                self.bump(1);
            }
            '\r' | '\n' | EOF => {
                return perror_at!(
                    Span::new(self.current_pos, self.current_pos + 1),
                    "Reached end of line in string"
                );
            }
            c => {
                ret = LexStringChar::Char(c);
                self.bump(1);
            }
        }

        Ok(ret)
    }

    /// Scans a numeric literal, consuming it and converting it to a token in
    /// the process.
    fn scan_numeric_literal(&mut self) -> PResult<Token> {
        let mut string = String::new();

        if self.current_char == '-' || self.current_char == '+' {
            if self.current_char == '-' {
                string.push(self.current_char);
            }

            self.bump(1);
        }

        while is_numeric(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        if self.current_char == '.' {
            string += ".";
            self.bump(1);
        }

        while is_numeric(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        if self.current_char == 'e' || self.current_char == 'E' {
            self.bump(1);
            string.push('e');

            if self.current_char == '+' || self.current_char == '-' {
                string.push(self.current_char);
                self.bump(1);
            }

            let mut expect_number = false;

            while is_numeric(self.current_char) {
                expect_number = true;
                string.push(self.current_char);
                self.bump(1);
            }

            if !expect_number {
                return perror!(
                    "Expected a numerical value following the exponential: {}",
                    string
                );
            }
        }

        debug!("Scanned Num `{}`", string);
        match string.parse::<f64>() {
            Ok(value) => Ok(Token::Num(value)),
            Err(_) => perror_at!(
                Span::new(self.current_pos, self.current_pos + 1),
                "Could not read Num literal: `{}`",
                string
            ),
        }
    }

    // Scans an identifier, unless it matches a keyword.
    fn scan_identifier_or_keyword(&mut self) -> PResult<Token> {
        let mut string = String::new();

        string.push(self.current_char);
        self.bump(1);

        while is_identifier_continuer(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        let token = match string.as_str() {
            "_" => Token::Underscore,

            "let" => Token::Let,
            "when" => Token::When,
            "operator" => Token::Operator,
            "fn" => Token::Fn,
            "type" => Token::Type,

            "nothing" => Token::Nothing,

            _ => match string.chars().nth(0).unwrap() {
                'A'..='Z' => Token::TypeId(string),
                'a'..='z' | '_' => Token::Id(string),
                _ => {
                    return perror_at!(
                        Span::new(self.current_pos, self.current_pos + 1),
                        "TODO: This should never happen, ever. `{}`",
                        string
                    );
                }
            },
        };

        Ok(token)
    }
}

impl<Chars: Iterator<Item = char>> Iterator for Lexer<Chars> {
    type Item = PResult<SpanToken>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.spanned_lex() {
            Ok((_, Token::Eof, _)) => None,
            t => Some(t),
        }
    }
}

/// Returns whether the character is a valid part of a number.
fn is_numeric(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

/// Returns whether the character is a valid beginning of an identifier.
fn is_identifier_start(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '_' => true,
        _ => false,
    }
}

/// Returns whether the character is a valid non-initial part of an identifier.
fn is_identifier_continuer(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '0'..='9' => true,
        '_' => true,
        _ => false,
    }
}

/// Returns whether the character would bound an operator's characters.
fn is_operator_boundary(c: char) -> bool {
    match c {
        ' ' | '\t' | '\r' | '\n' | EOF | '(' | ')' | '[' | ']' | '{' | '}' | '\\' | ',' | ';'
        | ':' | '"' => true,
        _ => false,
    }
}
