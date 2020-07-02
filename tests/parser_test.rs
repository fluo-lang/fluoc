#[cfg(test)]
mod lexer_tests {
    use lib::parser::parser::Parser;
    use lib::logger::logger::Logger;
    use std::path;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn lex_test() -> Result<(), std::vec::Vec<lib::logger::logger::Error<'static>>> {
        let path = path::Path::new("./tests/lolol_testing1234.fl");
        let file_contents = r#"-- lexer test (code shouldn't work)

def entry(one: int, two: int) {
    let x: int = 10;

    -- hello work 12342824\n924910djwi2wkfjar2riar
    /* ajwd asjf*/
    return 1+192*20/(120 + "2319")%10/203911234567890;



    let _qwertyuiopasdfghjklzxcvbnm: int;
    _qwertyuiopasdfghjklzxcvbnm = "hi";

}

/*
hlleoa1234567890qwertyuiopasdfghjklzxcvbnm,./?><;'":[]\|}{=-0987654321`~!@#$%^&*()_+
one two
djawd
sfghsdjajdksajfiwjijfa
*/

def _123awfawjfaifjaiwjf(one: int, two: int) -> float::int {


}
"#;
        let logger = Rc::new(RefCell::new(Logger::new(true)));
        logger.borrow_mut().add_file(path, file_contents);
        let mut parser = Parser::new(
            path,
            file_contents,
            Rc::clone(&logger)
        );
        parser.initialize_expr();
        parser.parse() 
    }
}
