mod operator_trie;
pub mod token;

use std::str::Chars;

use lookahead::{lookahead, Lookahead};

pub use self::operator_trie::OperatorTrie;
use self::token::{Token, TokenType};

use crate::{
    parser::result::{ParseError, ParseResult},
    util::{FileId, Span},
};

/// Mnemonic for the EOF end-of-file character.
const EOF: char = '\x00';

pub struct Lexer<'file, 'trie> {
    file_id: FileId,
    stream: Lookahead<Chars<'file>>,

    /// The trie determining max-munch operator chunky lip smack goodness.
    operator_trie: &'trie mut OperatorTrie,
    /// Whether the lexer should use the trie to lex operators, or if it should blindly lex them.
    use_trie: bool,

    /// The 0-indexed char position of the first char considered for the current token being lexed.
    start_pos: usize,
    /// The 0-indexed char position of `current_char`.
    current_pos: usize,
    /// The current character in the stream.
    current_char: char,
    /// The next character in the stream.
    next_char: char,

    /// Stack of string interpolation parentheses counts. Each stack position represents a string
    /// interpolation context. The value at each position represents how many open parens
    /// were encountered in the context.
    interp_parenthetical: Vec<usize>,
}

pub enum LexStringChar {
    Char(char),
    QuoteEnd,
    InterpolateBegin,
}

impl<'file, 'trie> Lexer<'file, 'trie> {
    pub fn new(
        file_id: FileId,
        source: &'file str,
        operator_trie: &'trie mut OperatorTrie,
    ) -> Lexer<'file, 'trie> {
        let mut lex = Lexer {
            file_id,
            stream: lookahead(source.chars()),
            operator_trie,
            use_trie: true,
            start_pos: 0,
            current_pos: 0,
            current_char: EOF,
            next_char: EOF,
            interp_parenthetical: vec![],
        };

        lex.bump(1);
        lex.current_pos = 0;
        lex
    }

    /// Adds an operator to the trie.
    pub fn add_operator(&mut self, operator: &str) {
        self.operator_trie.insert_operator(operator);
    }

    /// Checks whether the lexer has inserted the operator into the trie.
    pub fn has_operator(&self, operator: &str) -> bool {
        self.operator_trie.contains(operator)
    }

    /// Tells the lexer whether it should use its operator trie or not.
    pub fn should_use_trie(&mut self, should: bool) {
        self.use_trie = should;
    }

    pub fn current_span(&self) -> Span {
        Span::new(self.file_id, self.current_pos, self.current_pos + 1)
    }

    fn spanned_lex(&mut self) -> ParseResult<Token> {
        self.discard_whitespace_or_comments()?;

        self.start_pos = self.current_pos;
        let ty = self.lex()?;

        Ok(Token::new(
            Span::new(self.file_id, self.start_pos, self.current_pos),
            ty,
        ))
    }

    /// Cautiously bumps the input stream, avoiding reading past a '\n' character.
    fn bump(&mut self, n: usize) {
        for _ in 0..n {
            self.current_char = self.stream.next().unwrap_or(EOF);
            self.next_char = self.stream.lookahead(0).map_or(EOF, |&c| c);
            self.current_pos += 1;
        }
    }

    fn discard_whitespace_or_comments(&mut self) -> ParseResult<()> {
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

    fn scan_comment(&mut self) -> ParseResult<()> {
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
    fn lex(&mut self) -> ParseResult<TokenType> {
        let c = self.current_char;

        if c == EOF {
            Ok(TokenType::Eof)
        } else if c == '"' {
            self.scan_string()
        } else if c == '`' {
            self.scan_delimited_operator()
        } else if is_numeric(c) {
            self.scan_numeric_literal()
        } else if is_identifier_start(c) {
            self.scan_identifier_or_keyword()
        } else {
            match c {
                '\n' => {
                    self.bump(1);
                    Ok(TokenType::Newline)
                }
                '.' => {
                    if self.next_char == '.' {
                        self.bump(1);
                        if self.next_char == '.' {
                            self.bump(2);
                            Ok(TokenType::Ellipsis)
                        } else {
                            self.scan_custom_operator("..")
                        }
                    } else {
                        self.scan_custom_operator(".")
                    }
                }
                ',' => {
                    self.bump(1);
                    Ok(TokenType::Comma)
                }
                ':' => {
                    if self.next_char == ':' {
                        self.bump(2);
                        Ok(TokenType::ColonColon)
                    } else {
                        self.scan_atom()
                    }
                }
                ';' => {
                    self.bump(1);
                    Ok(TokenType::Semicolon)
                }
                '{' => {
                    self.bump(1);
                    Ok(TokenType::LCurly)
                }
                '}' => {
                    self.bump(1);
                    Ok(TokenType::RCurly)
                }
                '[' => {
                    self.bump(1);
                    Ok(TokenType::LSquare)
                }
                ']' => {
                    self.bump(1);
                    Ok(TokenType::RSquare)
                }
                '(' => {
                    self.bump(1);

                    if let Some(nest) = self.interp_parenthetical.last_mut() {
                        *nest += 1;
                    }

                    Ok(TokenType::LParen)
                }
                ')' => {
                    if let Some(0) = self.interp_parenthetical.last() {
                        self.scan_interp_continue()
                    } else {
                        if let Some(n) = self.interp_parenthetical.last_mut() {
                            *n -= 1;
                        }

                        self.bump(1);
                        Ok(TokenType::RParen)
                    }
                }
                '=' => {
                    if !is_operator_cont(self.next_char) {
                        self.bump(1);
                        Ok(TokenType::Eq)
                    } else {
                        self.scan_custom_operator("=")
                    }
                }
                '-' => {
                    if is_numeric(self.next_char) {
                        self.bump(1);
                        self.scan_numeric_literal()
                    } else if self.next_char == '>' {
                        self.bump(2);
                        Ok(TokenType::Arrow)
                    } else {
                        self.scan_custom_operator("-")
                    }
                }
                '+' => {
                    if is_numeric(self.next_char) {
                        self.bump(1);
                        self.scan_numeric_literal()
                    } else {
                        self.scan_custom_operator("+")
                    }
                }
                '\\' => {
                    self.bump(1);
                    Ok(TokenType::Backslash)
                }
                c => {
                    if is_operator_start(c) {
                        let leading = c.to_string();
                        self.scan_custom_operator(&leading)
                    } else {
                        self.bump(1);
                        Err(ParseError::UnknownChar {
                            c,
                            span: Span::new(self.file_id, self.current_pos, self.current_pos + 1),
                        })
                    }
                }
            }
        }
    }

    /// Scans a delimited operator ````op````.
    fn scan_delimited_operator(&mut self) -> ParseResult<TokenType> {
        self.bump(1); // Blindly consume the quote character
        let mut string = String::new();

        loop {
            match self.current_char {
                '`' => break,
                '\r' | '\n' | EOF => {
                    self.bump(1);
                    return Err(ParseError::EofInDelimitedOperator {
                        span: Span::new(self.file_id, self.current_pos, self.current_pos + 1),
                    });
                }
                c => {
                    string.push(c);
                    self.bump(1);
                }
            }
        }

        self.bump(1); // Bump the close quote.

        Ok(TokenType::Op(string))
    }

    /// Scans a custom operator.
    fn scan_custom_operator(&mut self, leading: &str) -> ParseResult<TokenType> {
        if !self.use_trie {
            return Ok(self.scan_arbitrary_operator(leading.to_owned()));
        }

        let mut operator = leading.to_owned();
        let mut node = self.operator_trie.find(leading);
        let mut last_end = node.and_then(|node| if node.end { Some(0) } else { None });
        let mut lookahead = 0usize;

        // Traverse the trie with each operator char.
        while node.is_some() {
            match self.stream.lookahead(lookahead) {
                Some(&c) => {
                    if !is_operator_cont(c) {
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
                self.bump(1); // Bump past the last leading character.
                for _ in 0..n {
                    operator.push(self.current_char);
                    self.bump(1);
                }

                Ok(TokenType::Op(operator))
            }
            _ => Ok(self.scan_arbitrary_operator(operator)),
        }
    }

    fn scan_arbitrary_operator(&mut self, leading: String) -> TokenType {
        let mut operator = leading;

        self.bump(1);
        while is_operator_cont(self.current_char) {
            operator.push(self.current_char);
            self.bump(1);
        }

        TokenType::Op(operator)
    }

    /// Scans an atom token.
    fn scan_atom(&mut self) -> ParseResult<TokenType> {
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
            Err(ParseError::EmptyAtom {
                span: Span::new(self.file_id, self.current_pos - 1, self.current_pos),
            })
        } else {
            Ok(TokenType::Atom(atom))
        }
    }

    /// Scans a new parsed string token.
    fn scan_string(&mut self) -> ParseResult<TokenType> {
        self.bump(1); // Blindly consume the quote character
        let mut string = String::new();

        loop {
            match self.scan_string_char()? {
                LexStringChar::Char(c) => {
                    string.push(c);
                }
                LexStringChar::QuoteEnd => {
                    return Ok(TokenType::Str(string));
                }
                LexStringChar::InterpolateBegin => {
                    self.interp_parenthetical.push(0);
                    return Ok(TokenType::InterpolateBegin(string));
                }
            }
        }
    }

    fn scan_interp_continue(&mut self) -> ParseResult<TokenType> {
        self.bump(1); // Blindly consume the rparen character
        let mut string = String::new();

        loop {
            match self.scan_string_char()? {
                LexStringChar::Char(c) => {
                    string.push(c);
                }
                LexStringChar::QuoteEnd => {
                    self.interp_parenthetical.pop();
                    return Ok(TokenType::InterpolateEnd(string));
                }
                LexStringChar::InterpolateBegin => {
                    return Ok(TokenType::InterpolateContinue(string));
                }
            }
        }
    }

    fn scan_string_char(&mut self) -> ParseResult<LexStringChar> {
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
                        return Err(ParseError::UnknownStringEscape {
                            c,
                            span: Span::new(self.file_id, self.current_pos, self.current_pos + 1),
                        });
                    }
                }
                self.bump(2);
            }
            '\"' => {
                ret = LexStringChar::QuoteEnd;
                self.bump(1);
            }
            '\r' | '\n' | EOF => {
                return Err(ParseError::EofInString {
                    span: Span::new(self.file_id, self.current_pos, self.current_pos + 1),
                });
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
    fn scan_numeric_literal(&mut self) -> ParseResult<TokenType> {
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
                return Err(ParseError::NoNumeralAfterExponential {
                    span: Span::new(self.file_id, self.current_pos, self.current_pos + 1),
                });
            }
        }

        debug!("Scanned Num `{}`", string);
        Ok(TokenType::Num(string))
    }

    // Scans an identifier, unless it matches a keyword.
    fn scan_identifier_or_keyword(&mut self) -> ParseResult<TokenType> {
        let mut string = String::new();

        string.push(self.current_char);
        self.bump(1);

        while is_identifier_continuer(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        let token = match string.as_str() {
            "_" => TokenType::Underscore,

            "let" => TokenType::Let,
            "when" => TokenType::When,
            "operator" => TokenType::Operator,
            "fn" => TokenType::Fn,
            "struct" => TokenType::Struct,

            "nothing" => TokenType::Nothing,

            _ => match string.chars().nth(0).unwrap() {
                'A'..='Z' => TokenType::TypeId(string),
                'a'..='z' | '_' => TokenType::Id(string),
                _ => unreachable!("ICE: non-identifier character found in identifier"),
            },
        };

        Ok(token)
    }
}

impl<'file> Iterator for Lexer<'file, '_> {
    type Item = ParseResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.spanned_lex() {
            Ok(Token {
                ty: TokenType::Eof, ..
            }) => None,
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

fn is_operator_start(c: char) -> bool {
    match c {
        '/'
        | '='
        | '-'
        | '+'
        | '!'
        | '*'
        | '%'
        | '<'
        | '>'
        | '&'
        | '|'
        | '^'
        | '~'
        | '?'
        | '$'
        | '#'
        | '@'
        | '\u{00a1}'..='\u{00a7}'
        | '\u{00a9}'
        | '\u{00ab}'
        | '\u{00ac}'
        | '\u{00ae}'
        | '\u{00b0}'..='\u{00b1}'
        | '\u{00b6}'
        | '\u{00bb}'
        | '\u{00bf}'
        | '\u{00d7}'
        | '\u{00f7}'
        | '\u{2016}'..='\u{2017}'
        | '\u{2020}'..='\u{2027}'
        | '\u{2030}'..='\u{203e}'
        | '\u{2041}'..='\u{2053}'
        | '\u{2055}'..='\u{205e}'
        | '\u{2190}'..='\u{23ff}'
        | '\u{2500}'..='\u{2775}'
        | '\u{2794}'..='\u{2bff}'
        | '\u{2e00}'..='\u{2e7f}'
        | '\u{3001}'..='\u{3003}'
        | '\u{3008}'..='\u{3020}'
        | '\u{3030}' => true,
        _ => false,
    }
}

fn is_operator_cont(c: char) -> bool {
    match c {
        '\u{0300}'..='\u{036F}'
        | '\u{1dc0}'..='\u{1dff}'
        | '\u{20d0}'..='\u{20ff}'
        | '\u{fe00}'..='\u{fe0f}'
        | '\u{fe20}'..='\u{fe2f}' => true,
        _ => is_operator_start(c),
    }
}
