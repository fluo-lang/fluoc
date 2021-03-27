module Syntax.LexerSpec where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )

import           Control.Monad.Except

import           Sources
import           Syntax.Token
import           Syntax.Lexer
import           Diagnostics

sid = SourceId 0
sn = Span sid

lexErr :: Int -> Char -> Diagnostic
lexErr c char = Diagnostic
  Error
  UnexpectedCharacterError
  [ Annotation (fromPos sid c)
               (Just $ "unexpected character `" ++ [char] ++ "`")
               Error
  ]
  (fromPos sid c)
  Nothing

spec :: Spec
spec = do
  it "should lex keywords"
    $          runExcept
                 (scanTokens sid "let import rec impl trait dec in if else match elif assign")
    `shouldBe` Right
                 [ MkToken (sn 0 3)   LetTok
                 , MkToken (sn 4 10)  ImportTok
                 , MkToken (sn 11 14) RecTok
                 , MkToken (sn 15 19) ImplTok
                 , MkToken (sn 20 25) TraitTok
                 , MkToken (sn 26 29) DecTok
                 , MkToken (sn 30 32) InTok
                 , MkToken (sn 33 35) IfTok
                 , MkToken (sn 36 40) ElseTok
                 , MkToken (sn 41 46) MatchTok
                 , MkToken (sn 47 51) ElifTok
                 , MkToken (sn 52 58) AssignTok
                 ]
  it "should lex symbols"
    $          runExcept (scanTokens sid "()\n[]\n{}\n")
    `shouldBe` Right
                 [ MkToken (sn 0 1)  LParenTok
                 , MkToken (sn 1 2)  RParenTok
                 , MkToken (sn 3 4)  LBracketTok
                 , MkToken (sn 4 5)  RBracketTok
                 , MkToken (sn 6 7)  LCurlyTok
                 , MkToken (sn 7 8)  RCurlyTok
                 ]
  it "should lex identifiers"
    $          runExcept (scanTokens sid "_123'? ahello? fold'")
    `shouldBe` Right
                 [ MkToken (sn 0 6) $ IdentTok "_123'?"
                 , MkToken (sn 7 14) $ IdentTok "ahello?"
                 , MkToken (sn 15 20) $ IdentTok "fold'"
                 ]
  it "should lex values regardless of whitespace"
    $          runExcept (scanTokens sid "1a23")
    `shouldBe` Right
                 [ MkToken (sn 0 1) $ IntegerTok 1
                 , MkToken (sn 1 4) $ IdentTok "a23"
                 ]
  it "should lex operators"
    $          runExcept (scanTokens sid "<$>+*-/|:$^@!~%&.=?")
    `shouldBe` Right [MkToken (sn 0 19) $ OperatorTok "<$>+*-/|:$^@!~%&.=?"]
  it "should lex integers"
    $          runExcept (scanTokens sid "1234567809")
    `shouldBe` Right [MkToken (sn 0 10) $ IntegerTok 1234567809]
  it "should lex floats"
    $          runExcept (scanTokens sid "1234567809.1234567809")
    `shouldBe` Right [MkToken (sn 0 21) $ FloatTok 1234567809.1234567809]
  it "should ignore comments"
    $          runExcept (scanTokens sid "# test test\n10\n/#\nmulti\n\n#/10")
    `shouldBe` Right
                 [ MkToken (sn 12 14) $ IntegerTok 10
                 , MkToken (sn 27 29) $ IntegerTok 10
                 ]
  it "should parse multiline comments properly"
    $          runExcept (scanTokens sid "/#\n  hello\n#/\n10\n/# bye #/")
    `shouldBe` Right [MkToken (sn 14 16) $ IntegerTok 10]
  it "should parse multiline comments properly"
    $          runExcept (scanTokens sid "/##/10/##/")
    `shouldBe` Right [MkToken (sn 4 6) $ IntegerTok 10]
  it "should parse an empty string"
    $          runExcept (scanTokens sid "\"\"")
    `shouldBe` Right [MkToken (sn 0 2) $ StrTok ""]
  it "should parse a string with spaces"
    $          runExcept (scanTokens sid "\"hello, world! fobar\"")
    `shouldBe` Right [MkToken (sn 0 21) $ StrTok "hello, world! fobar"]
  it "should parse a string with escapes"
    $          runExcept (scanTokens sid "\"\\n\\t\\DEL\\^C\"")
    `shouldBe` Right [MkToken (sn 0 13) $ StrTok "\n\t\DEL\^C"]
  it "should parse a string with escaped qoutes"
    $          runExcept (scanTokens sid "\"\\\"\"")
    `shouldBe` Right [MkToken (sn 0 4) $ StrTok "\""]
  it "should fail on bad escape code"
    $          runExcept (scanTokens sid "\"\\^c\"")
    `shouldBe` Left (lexErr 3 'c')
