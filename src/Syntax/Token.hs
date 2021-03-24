module Syntax.Token where

data TokenKind = IdentTok String
               | StrTok String
               | IntegerTok Integer
               | FloatTok Double
               | OperatorTok String

               | LetTok
               | ImportTok
               | RecTok
               | ImplTok
               | TraitTok
               | DecTok
               | InTok
               | IfTok
               | ElseTok
               | ElifTok
               | MatchTok

               | DoubleQouteTok
               | SingleQouteTok

               | LBracketTok
               | RBracketTok
               | LParenTok
               | RParenTok
               | LCurlyTok
               | RCurlyTok

               | BreakTok
               | EofTok
               deriving (Eq, Show)

getStaticTok tok = case tok of
  LetTok         -> "let"
  ImportTok      -> "import"
  RecTok         -> "rec"
  ImplTok        -> "impl"
  TraitTok       -> "trait"
  DecTok         -> "dec"
  InTok          -> "in"
  IfTok          -> "if"
  ElseTok        -> "else"
  ElifTok        -> "elif"
  MatchTok       -> "match"

  DoubleQouteTok -> "\""
  SingleQouteTok -> "'"

  LBracketTok    -> "["
  RBracketTok    -> "]"
  LParenTok      -> "("
  RParenTok      -> ")"
  LCurlyTok      -> "{"
  RCurlyTok      -> "}"

class Display a where
  display :: a -> String

instance Display TokenKind where
  display tok = case tok of
    IdentTok    val -> "identifier `" ++ val ++ "`"
    StrTok      val -> "string `" ++ val ++ "`"
    IntegerTok  val -> "integer `" ++ show val ++ "`"
    FloatTok    val -> "float `" ++ show val ++ "`"
    OperatorTok val -> "operator `" ++ val ++ "`"
    EofTok          -> "end of file"
    BreakTok        -> "line break"

    _               -> "token `" ++ getStaticTok tok ++ "`"
