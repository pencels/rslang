pub mod ast;
pub mod builtins;
pub mod parselet;
pub mod prec;
pub mod result;

use std::{cmp::Ordering, collections::HashMap, sync::Arc};

use prec::TryAddLtError;

use crate::{
    lexer::{
        token::{Token, TokenType},
        Lexer,
    },
    util::{FileId, Span},
};

use self::{
    ast::{Defn, DefnKind, Expr, Pattern, PrecConstraint, PrecConstraintKind, Spanned},
    builtins::{
        expr::{BinOpParselet, PostfixOpParselet, PrefixOpParselet},
        BOTTOM_OP, POSTFIX_OP, PREFIX_OP,
    },
    parselet::{PatternAction, PostfixAction, PrefixAction},
    prec::{Associativity, Fixity, Poset},
    result::{ParseError, ParseResult},
};

pub struct Parser<'file, 'trie, 'prec> {
    /// The file id tied to the file this parser is parsing.
    file_id: FileId,
    /// The lexer over the file.
    pub lexer: Lexer<'file, 'trie>,
    /// The next token.
    next_token: Option<Token>,

    poset: &'prec mut Poset<Token>,
    prefix_actions: &'prec mut HashMap<Token, Arc<dyn PrefixAction>>,
    postfix_actions: &'prec mut HashMap<Token, (Option<Associativity>, Arc<dyn PostfixAction>)>,
    pattern_actions: &'prec mut HashMap<Token, Arc<dyn PatternAction>>,
}

impl<'file, 'trie, 'prec> Parser<'file, 'trie, 'prec> {
    pub fn new(
        file_id: FileId,
        lexer: Lexer<'file, 'trie>,
        poset: &'prec mut Poset<Token>,
        prefix_actions: &'prec mut HashMap<Token, Arc<dyn PrefixAction>>,
        postfix_actions: &'prec mut HashMap<Token, (Option<Associativity>, Arc<dyn PostfixAction>)>,
        pattern_actions: &'prec mut HashMap<Token, Arc<dyn PatternAction>>,
    ) -> Parser<'file, 'trie, 'prec> {
        Parser {
            file_id,
            lexer,
            next_token: None,
            poset,
            prefix_actions,
            postfix_actions,
            pattern_actions,
        }
    }

    pub fn add_operator(
        &mut self,
        fixity: &Spanned<Fixity>,
        assoc: &Option<Spanned<Associativity>>,
        ops: &[Token],
        constraints: &[PrecConstraint],
    ) -> ParseResult<()> {
        for op in ops {
            self.lexer.add_operator(op.ty.repr());

            // Register parselets.
            match fixity.item {
                Fixity::Prefix => {
                    if let Some((original, _)) = self.prefix_actions.get_key_value(op) {
                        return Err(ParseError::RedeclaringOp {
                            span: op.span,
                            original_span: original.span,
                            was: "prefix",
                            now: "prefix",
                        });
                    }
                    self.prefix_actions.insert(
                        op.clone(),
                        Arc::new(PrefixOpParselet) as Arc<dyn PrefixAction>,
                    );
                }
                Fixity::Infix => {
                    if let Some((original, _)) = self.postfix_actions.get_key_value(op) {
                        return Err(ParseError::RedeclaringOp {
                            span: op.span,
                            original_span: original.span,
                            was: "postfix or infix",
                            now: "infix",
                        });
                    }
                    self.postfix_actions.insert(
                        op.clone(),
                        (
                            assoc.clone().map(|a| a.item),
                            Arc::new(BinOpParselet) as Arc<dyn PostfixAction>,
                        ),
                    );
                    // Insert infix ops under prefix control operator.
                    self.poset
                        .try_add_lt(op.clone(), PREFIX_OP.clone())
                        .unwrap();
                }
                Fixity::Postfix => {
                    if let Some((original, _)) = self.postfix_actions.get_key_value(op) {
                        return Err(ParseError::RedeclaringOp {
                            span: op.span,
                            original_span: original.span,
                            was: "postfix or infix",
                            now: "infix",
                        });
                    }
                    self.postfix_actions.insert(
                        op.clone(),
                        (None, Arc::new(PostfixOpParselet) as Arc<dyn PostfixAction>),
                    );
                    // Insert postfix ops as the postfix control operator.
                    self.poset
                        .try_add_eq(op.clone(), POSTFIX_OP.clone())
                        .unwrap();
                }
            }

            // Register precedence.
            // Operators will always be above the bottom-most prec level.
            self.poset
                .try_add_lt(BOTTOM_OP.clone(), op.clone())
                .unwrap();
            for constraint in constraints {
                let other_op = &constraint.op;
                match constraint.kind.item {
                    PrecConstraintKind::Above => {
                        match self.poset.try_add_lt(other_op.clone(), op.clone()) {
                            Ok(_) => {}
                            Err(TryAddLtError::Cycle) => {
                                return Err(ParseError::PrecConstraintAddsCycle {
                                    span: constraint.kind.span.unite(constraint.op.span),
                                })
                            }
                        }
                    }
                    PrecConstraintKind::With => {
                        match self.poset.try_add_eq(op.clone(), other_op.clone()) {
                            Ok(_) => {}
                            Err(prec::TryAddEqError::Cycle) => {
                                return Err(ParseError::PrecConstraintAddsCycle {
                                    span: constraint.kind.span.unite(constraint.op.span),
                                })
                            }
                        }
                    }
                    PrecConstraintKind::Below => {
                        match self.poset.try_add_lt(op.clone(), other_op.clone()) {
                            Ok(_) => {}
                            Err(prec::TryAddLtError::Cycle) => {
                                return Err(ParseError::PrecConstraintAddsCycle {
                                    span: constraint.kind.span.unite(constraint.op.span),
                                })
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Peek the next token.
    pub fn peek(&mut self) -> ParseResult<Option<Token>> {
        match &mut self.next_token {
            Some(tok) => Ok(Some(tok.clone())),
            next_token @ None => match self.lexer.next() {
                Some(maybe_tok) => {
                    let tok = maybe_tok?;
                    *next_token = Some(tok.clone());
                    Ok(Some(tok))
                }
                None => Ok(None),
            },
        }
    }

    /// Peek the next token, erroring out if at EOF.
    pub fn peek_expect(&mut self) -> ParseResult<Token> {
        match self.peek()? {
            Some(tok) => Ok(tok),
            None => Err(ParseError::UnexpectedEof {
                span: self.lexer.current_span(),
            }),
        }
    }

    /// Peek the next token's type.
    pub fn peek_ty(&mut self) -> ParseResult<Option<TokenType>> {
        Ok(self.peek()?.map(|tok| tok.ty))
    }

    /// Peek the next token's type, erroring out if at EOF.
    pub fn peek_expect_ty(&mut self) -> ParseResult<TokenType> {
        Ok(self.peek_expect()?.ty)
    }

    /// Advance the token stream by one token.
    pub fn eat(&mut self) -> ParseResult<Token> {
        let tok = self.peek_expect()?;
        self.next_token
            .take()
            .expect("ICE: called eat() with no next token");
        Ok(tok)
    }

    /// Tries to advance the token stream if the current token matches the TokenType.
    pub fn try_eat(&mut self, ty: TokenType) -> ParseResult<bool> {
        match self.peek_ty()? {
            Some(tty) => Ok(tty == ty),
            _ => Ok(false),
        }
    }

    /// Advance the token stream by one token, expecting that the given token type will be present.
    pub fn eat_expect(&mut self, ty: TokenType) -> ParseResult<Token> {
        let tok = self.eat()?;
        if tok.ty == ty {
            Ok(tok)
        } else {
            Err(ParseError::Expected {
                span: tok.span,
                expected: format!("{:?}", ty),
                got: tok,
            })
        }
    }

    /// Skips past newlines. Use for contexts where zero or more newlines can be inserted between tokens.
    fn skip_newlines(&mut self) -> ParseResult<()> {
        loop {
            match self.peek_ty()? {
                Some(TokenType::Newline) => {
                    self.eat()?;
                }
                Some(_) => break,
                None => return Err(ParseError::Incomplete),
            }
        }

        Ok(())
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let peeked = self.peek_expect()?;
        let action = Self::token_or_fail("pattern", &self.pattern_actions, &peeked)?;
        action.parse(self)
    }

    fn token_or_fail<T: Clone>(
        what: &str,
        actions: &HashMap<Token, T>,
        peeked: &Token,
    ) -> ParseResult<T> {
        match actions.get(&peeked) {
            Some(action) => Ok(action.clone()),
            None => {
                let mut expected_tokens = String::new();
                for (i, k) in actions.keys().enumerate() {
                    let at_end = i == actions.len() - 1;

                    if at_end && i == 1 {
                        expected_tokens.push_str(" or ");
                    } else if at_end {
                        expected_tokens.push_str(", or ");
                    } else if i > 0 {
                        expected_tokens.push_str(", ");
                    }

                    expected_tokens.push_str(k.ty.repr());
                }

                return Err(ParseError::Expected {
                    span: peeked.span,
                    expected: expected_tokens,
                    got: peeked.clone(),
                });
            }
        }
    }

    fn maybe_peek_expr_continue(
        &mut self,
        op: &Token,
    ) -> ParseResult<Option<Arc<dyn PostfixAction>>> {
        if let Some(peeked_op) = self.peek()? {
            let (assoc, action) =
                self.postfix_actions
                    .get(&peeked_op)
                    .ok_or_else(|| ParseError::UnknownOp {
                        span: peeked_op.span,
                        op: peeked_op.ty.repr().to_string(),
                    })?;

            match self.poset.cmp(&peeked_op, op) {
                Some(Ordering::Greater) => Ok(Some(action.clone())),
                Some(Ordering::Equal) => match assoc {
                    Some(Associativity::Left) => Ok(None),
                    Some(Associativity::Right) => Ok(Some(action.clone())),
                    _ => Err(ParseError::NonAssocOperator {
                        span: peeked_op.span,
                    }),
                },
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_expr(&BOTTOM_OP)
    }

    fn parse_expr(&mut self, op: &Token) -> ParseResult<Expr> {
        let peeked = self.peek_expect()?;
        let mut lhs =
            Self::token_or_fail("expression", &self.prefix_actions, &peeked)?.parse(self)?;

        while let Some(action) = self.maybe_peek_expr_continue(op)? {
            lhs = action.parse(self, Arc::new(lhs))?;
        }

        Ok(lhs)
    }

    fn parse_operator_defn(&mut self) -> ParseResult<Defn> {
        self.eat()?; // Blindly eat the "operator" keyword.

        let fixity = self.parse_fixity()?;
        let assoc = if fixity.item == Fixity::Infix {
            Some(self.parse_associativity()?)
        } else {
            None
        };
        let ops = self.parse_new_operators()?;
        let constraints = self.parse_operator_constraints()?;

        Ok(Defn {
            kind: DefnKind::Operator {
                fixity,
                assoc,
                ops,
                constraints,
            },
            span: Span::dummy(),
        })
    }

    fn parse_operator_constraints(&mut self) -> ParseResult<Vec<PrecConstraint>> {
        let mut constraints = Vec::new();

        loop {
            match self.peek()? {
                Some(tok) => match tok.ty {
                    TokenType::Id(constraint) => {
                        constraints.push(self.parse_operator_constraint(&constraint)?);
                    }
                    TokenType::Newline => break,
                    _ => {
                        return Err(ParseError::Expected {
                            span: tok.span,
                            expected: "a precedence constraint ('above', 'with', 'below')"
                                .to_owned(),
                            got: tok,
                        })
                    }
                },
                None => break,
            }
        }

        Ok(constraints)
    }

    fn parse_operator_constraint(&mut self, constraint: &str) -> ParseResult<PrecConstraint> {
        let tok = self.eat()?; // Eat the constraint word.
        let op = self.parse_existing_operator()?;
        let kind = match constraint {
            "above" => PrecConstraintKind::Above,
            "with" => PrecConstraintKind::With,
            "below" => PrecConstraintKind::Below,
            _ => {
                return Err(ParseError::Expected {
                    span: tok.span,
                    expected: "a precedence constraint ('above', 'with', 'below')".to_owned(),
                    got: tok,
                })
            }
        };

        Ok(PrecConstraint {
            op,
            kind: Spanned::new(tok.span, kind),
        })
    }

    fn parse_existing_operator(&mut self) -> ParseResult<Token> {
        let tok = self.eat()?; // Eat potential operator.
        match tok.ty {
            TokenType::Op(_) => Ok(tok),
            _ => Err(ParseError::Expected {
                span: tok.span,
                expected: "an operator".to_owned(),
                got: tok,
            }),
        }
    }

    fn parse_new_operators(&mut self) -> ParseResult<Vec<Token>> {
        // Switch off the trie, so that the lexer is open to reading an unseen operator.
        self.lexer.should_use_trie(false);

        let mut ops = Vec::new();
        let tok = self.eat()?;
        match tok.ty {
            TokenType::Op(_) => {
                ops.push(tok);
            }
            _ => {
                return Err(ParseError::Expected {
                    span: tok.span,
                    expected: "an operator".to_owned(),
                    got: tok,
                })
            }
        }

        while let Some(TokenType::Op(_)) = self.peek_ty()? {
            let tok = self.eat()?;
            ops.push(tok);
        }

        // Switch the trie back on.
        self.lexer.should_use_trie(true);

        Ok(ops)
    }

    fn parse_contextual_keywords<T, F>(&mut self, map: F) -> ParseResult<Result<Spanned<T>, Token>>
    where
        F: FnOnce(&str) -> Option<T>,
    {
        let tok = self.eat()?;
        Ok(match tok.ty {
            TokenType::Id(ref word) => match map(word) {
                Some(v) => Ok(Spanned::new(tok.span, v)),
                None => Err(tok),
            },
            _ => Err(tok),
        })
    }

    fn parse_associativity(&mut self) -> ParseResult<Spanned<Associativity>> {
        self.parse_contextual_keywords(|word| match word {
            "left" => Some(Associativity::Left),
            "right" => Some(Associativity::Right),
            "nonassoc" => Some(Associativity::NonAssoc),
            _ => None,
        })?
        .map_err(|tok| ParseError::Expected {
            span: tok.span,
            expected: "an associativity ('left', 'right', or 'nonassoc')".to_owned(),
            got: tok,
        })
    }

    fn parse_fixity(&mut self) -> ParseResult<Spanned<Fixity>> {
        self.parse_contextual_keywords(|word| match word {
            "prefix" => Some(Fixity::Prefix),
            "infix" => Some(Fixity::Infix),
            "postfix" => Some(Fixity::Postfix),
            _ => None,
        })?
        .map_err(|tok| ParseError::Expected {
            span: tok.span,
            expected: "a fixity ('prefix', 'infix', or 'postfix')".to_owned(),
            got: tok,
        })
    }

    fn parse_struct_defn(&mut self) -> ParseResult<Defn> {
        todo!()
    }

    fn parse_expression_defn(&mut self) -> ParseResult<Defn> {
        let expr = self.parse_expression()?;
        Ok(Defn {
            span: expr.span,
            kind: DefnKind::Expr(expr),
        })
    }

    pub fn parse_next_defn(&mut self) -> ParseResult<Option<Defn>> {
        Ok(match self.peek_ty()? {
            Some(ty) => Some(match ty {
                TokenType::Operator => self.parse_operator_defn()?,
                TokenType::Struct => self.parse_struct_defn()?,
                _ => self.parse_expression_defn()?,
            }),
            None => None,
        })
    }
}
