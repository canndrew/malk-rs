/// A position in a text document.
#[derive(Debug, Clone, Copy)]
pub struct TextPos {
    /// The column number (in chars)
    pub col: usize,
    /// The line number (in chars)
    pub line: usize,
    /// The byte position
    pub byte: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: TextPos,
    pub end: TextPos,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Word(String),
    Whitespace,
    Parens(Vec<Token>),
    CurlyBraces(Vec<Token>),
    SquareBraces(Vec<Token>),
    Equals,
    Comma,
    Colon,
    Semicolon,
    Hash,
    Minus,
    GreaterThan,
    At,
}

impl TextPos {
    /// Create a TextPos at the start of a document.
    pub fn start() -> TextPos {
        TextPos {
            col: 0,
            line: 0,
            byte: 0,
        }
    }

    /// Advance a position by one character. Returns the character that was advanced past and the
    /// new position.
    pub fn next(self, src: &str) -> Option<(char, TextPos)> {
        let mut iter = src[self.byte..].char_indices();
        let (i, c) = match iter.next() {
            Some((_, c)) => {
                match iter.next() {
                    Some((i, _)) => (self.byte + i, c),
                    None => (src.len(), c),
                }
            },
            None => return None,
        };

        let pos = if c == '\n' {
            TextPos {
                col: 0,
                line: self.line + 1,
                byte: i,
            }
        }
        else {
            TextPos {
                col: self.col + 1,
                line: self.line,
                byte: i,
            }
        };
        Some((c, pos))
    }
}

impl Token {
    pub fn is_whitespace(&self) -> bool {
        if let TokenKind::Whitespace = self.kind {
            true
        }
        else {
            false
        }
    }
}

/*
impl Span {
    pub fn from_tokens(tokens: &[Token]) -> Span {
        let iter = tokens.enumerate().iter();
        loop {
            match iter.next() {
                Some((i, token)) => {
                    if !token.is_whitespace() {
                        let start = i;
                        let mut end = i;
                        for (i, token) in iter {
                            if !token.is_whitespace() {
                                end = i;
                            }
                        }
                        return Span {
                            start: tokens[start].span.start,
                            end: tokens[end].span.end,
                        };
                    }
                },
                None => return Span {
                    start: 
            }
        }
    }
}
*/

quick_error! {
    #[derive(Debug)]
    pub enum LexError {
        UnclosedDelimiter {
            open_pos: TextPos
        } {
            description("Unclosed delimiter")
        }
        InvalidClosingDelimiter {
            open_pos: TextPos,
            close_pos: TextPos
        } {
            description("Invalid closing delimiter")
        }
        UnexpectedChar {
            c: char,
            pos: TextPos
        } {
            description("Unexpected character")
        }
        UnexpectedClosingDelimiter {
            pos: TextPos
        } {
            description("Unexpected closing delimiter")
        }
    }
}

struct SubLex {
    tokens: Vec<Token>,
    terminator: Option<(char, TextPos)>,
    final_pos: TextPos,
}

fn sub_lex(start: TextPos, src: &str) -> Result<SubLex, LexError> {
    let mut tokens = Vec::new();
    let mut pos = start;
    loop {
        let (c, p) = match pos.next(src) {
            Some(x) => x,
            None => return Ok(SubLex {
                tokens: tokens,
                terminator: None,
                final_pos: pos,
            }),
        };
        debug!("sub_lex(pos = {:?}, p = {:?}, c = {:?}, ...)", pos, p, c);
        if c.is_whitespace() {
            let mut end = p;
            loop {
                let (c, p) = match end.next(src) {
                    Some(x) => x,
                    None => break,
                };
                if !c.is_whitespace() {
                    break;
                }
                end = p;
            }

            let token = Token {
                kind: TokenKind::Whitespace,
                span: Span {
                    start: pos,
                    end: end,
                },
            };

            tokens.push(token);
            pos = end;
            continue;
        }
        if c.is_alphabetic() {
            let mut end = p;
            loop {
                let (c, p) = match end.next(src) {
                    Some(x) => x,
                    None => break,
                };
                if !c.is_alphabetic() {
                    break;
                }
                end = p;
            }

            let token = Token {
                kind: TokenKind::Word(String::from(&src[pos.byte..end.byte])),
                span: Span {
                    start: pos,
                    end: end,
                },
            };

            tokens.push(token);
            pos = end;
            continue;
        }
        {
            let mut lex_delim = |terminator, f: fn(Vec<Token>) -> TokenKind| {
                let sub = try!(sub_lex(p, src));
                match sub.terminator {
                    Some((c, dp)) => {
                        if c == terminator {
                            let token = Token {
                                kind: f(sub.tokens),
                                span: Span {
                                    start: pos,
                                    end: sub.final_pos,
                                },
                            };
                            tokens.push(token);
                            pos = sub.final_pos;
                            Ok(())
                        }
                        else {
                            return Err(LexError::InvalidClosingDelimiter {
                                open_pos: pos,
                                close_pos: dp,
                            });
                        }
                    }
                    None => {
                        return Err(LexError::UnclosedDelimiter {
                            open_pos: pos,
                        });
                    },
                }
            };
            if c == '(' {
                try!(lex_delim(')', TokenKind::Parens));
                continue;
            }
            if c == '{' {
                try!(lex_delim('}', TokenKind::CurlyBraces));
                continue;
            }
            if c == '[' {
                try!(lex_delim(']', TokenKind::SquareBraces));
                continue;
            }
        }
        if c == ')' || c == '}' || c == ']' {
            return Ok(SubLex {
                tokens: tokens,
                terminator: Some((c, pos)),
                final_pos: p,
            });
        }
        {
            let mut lex_symbol = |kind| {
                let token = Token {
                    kind: kind,
                    span: Span {
                        start: pos,
                        end: p,
                    },
                };
                tokens.push(token);
                pos = p;
            };
            if c == '#' {
                lex_symbol(TokenKind::Hash);
                continue;
            }
            if c == '=' {
                lex_symbol(TokenKind::Equals);
                continue;
            }
            if c == ';' {
                lex_symbol(TokenKind::Semicolon);
                continue;
            }
            if c == ',' {
                lex_symbol(TokenKind::Comma);
                continue;
            }
            if c == ':' {
                lex_symbol(TokenKind::Colon);
                continue;
            }
            if c == '-' {
                lex_symbol(TokenKind::Minus);
                continue;
            }
            if c == '>' {
                lex_symbol(TokenKind::GreaterThan);
                continue;
            }
            if c == '@' {
                lex_symbol(TokenKind::At);
                continue;
            }
        }
        return Err(LexError::UnexpectedChar {
            c: c,
            pos: pos,
        });
    }
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let pos = TextPos::start();
    let sub = try!(sub_lex(pos, src));
    match sub.terminator {
        None => return Ok(sub.tokens),
        Some((_, dp)) => return Err(LexError::UnexpectedClosingDelimiter {
            pos: dp,
        }),
    };
}

/*
#[cfg(test)]
mod test {
    use lexer::lex;

    #[test]
    fn test() {
        let text = "WOW (some words { } [ floo () ] )"
        let lexed = unwrap!(lex(text));
        let lexed = lexed.map(|t| t.kind);
        assert_eq!(lexed, &[
            TokenKind::Word, TokenKind::Parens(
    }
}
*/

