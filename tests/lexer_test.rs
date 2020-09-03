use lib::helpers::Pos;
use lib::lexer::TokenType;
use lib::lexer::*;
use lib::sourcemap::SourceMapInner;

use std::path;

macro_rules! lex_assert {
    ($source: expr, $token: expr, $name: ident) => {
        #[test]
        fn $name() {
            let sourcemap = SourceMapInner::new();
            let filename_id = sourcemap
                .borrow_mut()
                .insert_file(path::PathBuf::from("test_fl.fl"), $source.to_string());

            let mut l = Lexer::new(filename_id, sourcemap);
            assert_eq!(l.advance().map(|tok| tok.token), Ok($token))
        }
    };
}

lex_assert!(" true", TokenType::True, true_test);
lex_assert!(" false", TokenType::False, false_test);

lex_assert!(" \"hello \"", TokenType::String, string_test);
lex_assert!(
    " \"12d8a9fh3nffandjs \\\" \"",
    TokenType::String,
    string_escape_test
);

lex_assert!(" def", TokenType::Def, def_test);
lex_assert!(" type", TokenType::Type, type_test);
lex_assert!(" impl", TokenType::Impl, impl_test);
lex_assert!(" pattern", TokenType::Pattern, pattern_test);
lex_assert!(" unit", TokenType::Unit, unit_test);
lex_assert!(" let", TokenType::Let, let_test);
lex_assert!(" as", TokenType::As, as_test);
lex_assert!(" is", TokenType::Is, is_test);
lex_assert!(" return", TokenType::Return, return_test);
lex_assert!(" yield", TokenType::Yield, yield_test);
lex_assert!(" pub", TokenType::Public, public_test);
lex_assert!(" extern", TokenType::Extern, extern_test);

lex_assert!(" @", TokenType::At, at_test);

lex_assert!(" if", TokenType::If, if_test);
lex_assert!(" else", TokenType::Else, else_test);

lex_assert!(" _13292_293dh_238", TokenType::Identifier, ident_test_1);
lex_assert!(" i1dw23", TokenType::Identifier, ident_test_2);

lex_assert!(" 1", TokenType::Number, number_test_1);
lex_assert!(" 1287321234567890", TokenType::Number, number_test_2);

lex_assert!(" /", TokenType::Div, div_test);
lex_assert!(" %", TokenType::Mod, mod_test);
lex_assert!(" *", TokenType::Mul, mul_test);
lex_assert!(" +", TokenType::Add, add_test);
lex_assert!(" -", TokenType::Sub, sub_test);
lex_assert!(" %%", TokenType::DMod, dmod_test);

lex_assert!(" >", TokenType::GT, gt_test);
lex_assert!(" <", TokenType::LT, lt_test);
lex_assert!(" >=", TokenType::GE, ge_test);
lex_assert!(" <=", TokenType::LE, le_test);
lex_assert!(" ==", TokenType::EQ, eq_test);

lex_assert!(" ->", TokenType::Arrow, arrow_test);
lex_assert!(" =>", TokenType::FatArrow, fat_arrow_test);

lex_assert!(" (", TokenType::LP, lp_test);
lex_assert!(" )", TokenType::RP, rp_test);
lex_assert!(" {", TokenType::LCP, lcp_test);
lex_assert!(" }", TokenType::RCP, rcp_test);
lex_assert!(" [", TokenType::LB, lb_test);
lex_assert!(" ]", TokenType::RB, rb_test);
lex_assert!(" ?", TokenType::Question, question_test);
lex_assert!(" .", TokenType::Dot, dot_test);
lex_assert!(" =", TokenType::Equals, equals_test);
lex_assert!(" :", TokenType::Colon, color_test);
lex_assert!(" ::", TokenType::DoubleColon, double_colon_test);
lex_assert!(" $", TokenType::Dollar, dollar_test);
lex_assert!(" ;", TokenType::Semi, semi_test);
lex_assert!(" ,", TokenType::Comma, comma_test);
lex_assert!(" _", TokenType::Underscore, underscore_test);
lex_assert!("", TokenType::EOF, eof_test);
