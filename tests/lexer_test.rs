#[cfg(test)]
mod lexer_tests {
    use lib::helpers::Pos;
    use lib::lexer::TokenType::*;
    use lib::lexer::*;

    #[test]
    fn lex_test() -> Result<(), ()> {
        let mut l = Lexer::new(
            "./tests/lexer_test.fluo",
            r#"-- lexer test (code shouldn't work)

def entry(one, two) {
    let x: int = 10;

    -- hello work 12342824\n924910djwi2wkfjar2riar
    /* ajwd asjf*/
    return 1+192*20/(120 + "2319")%10/203911234567890;



    let _qwertyuiopasdfghjklzxcvbnm;
    _qwertyuiopasdfghjklzxcvbnm = "hi";

}

/*
hlleoa1234567890qwertyuiopasdfghjklzxcvbnm,./?><;'":[]\|}{=-0987654321`~!@#$%^&*()_+
one two
djawd
sfghsdjajdksajfiwjijfa
*/

def _123awfawjfaifjaiwjf(one, two) {


}
"#,
        );

        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: DEF,
                pos: Pos {
                    s: 37,
                    e: 40,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("entry"),
                pos: Pos {
                    s: 41,
                    e: 46,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LP,
                pos: Pos {
                    s: 46,
                    e: 47,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("one"),
                pos: Pos {
                    s: 47,
                    e: 50,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: COMMA,
                pos: Pos {
                    s: 50,
                    e: 51,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("two"),
                pos: Pos {
                    s: 52,
                    e: 55,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: RP,
                pos: Pos {
                    s: 55,
                    e: 56,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LCP,
                pos: Pos {
                    s: 57,
                    e: 58,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LET,
                pos: Pos {
                    s: 63,
                    e: 66,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("x"),
                pos: Pos {
                    s: 67,
                    e: 68,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: COLON,
                pos: Pos {
                    s: 68,
                    e: 69,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("int"),
                pos: Pos {
                    s: 70,
                    e: 73,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: EQUALS,
                pos: Pos {
                    s: 74,
                    e: 75,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("10"),
                pos: Pos {
                    s: 76,
                    e: 78,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: SEMI,
                pos: Pos {
                    s: 78,
                    e: 79,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: RETURN,
                pos: Pos {
                    s: 155,
                    e: 161,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("1"),
                pos: Pos {
                    s: 162,
                    e: 163,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: ADD,
                pos: Pos {
                    s: 163,
                    e: 164,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("192"),
                pos: Pos {
                    s: 164,
                    e: 167,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: MUL,
                pos: Pos {
                    s: 167,
                    e: 168,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("20"),
                pos: Pos {
                    s: 168,
                    e: 170,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: DIV,
                pos: Pos {
                    s: 170,
                    e: 171,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LP,
                pos: Pos {
                    s: 171,
                    e: 172,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("120"),
                pos: Pos {
                    s: 172,
                    e: 175,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: ADD,
                pos: Pos {
                    s: 176,
                    e: 177,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: STRING("\"2319\""),
                pos: Pos {
                    s: 178,
                    e: 184,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: RP,
                pos: Pos {
                    s: 184,
                    e: 185,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: MOD,
                pos: Pos {
                    s: 185,
                    e: 186,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("10"),
                pos: Pos {
                    s: 186,
                    e: 188,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: DIV,
                pos: Pos {
                    s: 188,
                    e: 189,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: NUMBER("203911234567890"),
                pos: Pos {
                    s: 189,
                    e: 204,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: SEMI,
                pos: Pos {
                    s: 204,
                    e: 205,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LET,
                pos: Pos {
                    s: 213,
                    e: 216,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("_qwertyuiopasdfghjklzxcvbnm"),
                pos: Pos {
                    s: 217,
                    e: 244,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: SEMI,
                pos: Pos {
                    s: 244,
                    e: 245,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("_qwertyuiopasdfghjklzxcvbnm"),
                pos: Pos {
                    s: 250,
                    e: 277,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: EQUALS,
                pos: Pos {
                    s: 278,
                    e: 279,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: STRING("\"hi\""),
                pos: Pos {
                    s: 280,
                    e: 284,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: SEMI,
                pos: Pos {
                    s: 284,
                    e: 285,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: RCP,
                pos: Pos {
                    s: 287,
                    e: 288,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: DEF,
                pos: Pos {
                    s: 419,
                    e: 422,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("_123awfawjfaifjaiwjf"),
                pos: Pos {
                    s: 423,
                    e: 443,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LP,
                pos: Pos {
                    s: 443,
                    e: 444,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("one"),
                pos: Pos {
                    s: 444,
                    e: 447,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: COMMA,
                pos: Pos {
                    s: 447,
                    e: 448,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: IDENTIFIER("two"),
                pos: Pos {
                    s: 449,
                    e: 452,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: RP,
                pos: Pos {
                    s: 452,
                    e: 453,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: LCP,
                pos: Pos {
                    s: 454,
                    e: 455,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        assert_eq!(
            l.advance().unwrap(),
            Token {
                token: RCP,
                pos: Pos {
                    s: 458,
                    e: 459,
                    filename: "./tests/lexer_test.fluo"
                }
            }
        );
        Ok(())
    }
}
