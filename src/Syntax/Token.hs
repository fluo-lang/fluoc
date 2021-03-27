module Syntax.Token where

data TokenKind = IdentTok String
               | StrTok String
               | IntegerTok Integer
               | FloatTok Double
               | OperatorTok String
               | PolyTok String

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
               | AssignTok
               | OpDefTok

               | DoubleQouteTok
               | SingleQouteTok

               | LBracketTok
               | RBracketTok
               | LParenTok
               | RParenTok
               | LCurlyTok
               | RCurlyTok

               | EofTok
               deriving (Eq, Show)

getStaticTok :: TokenKind -> String
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
  AssignTok      -> "assign"

  DoubleQouteTok -> "\""
  SingleQouteTok -> "'"

  LBracketTok    -> "["
  RBracketTok    -> "]"
  LParenTok      -> "("
  RParenTok      -> ")"
  LCurlyTok      -> "{"
  RCurlyTok      -> "}"

  OpDefTok       -> "opdef"

  _              -> error "Missing case, internal error. Token.hs: getStaticTok"

class Display a where
  display :: a -> String

instance Display TokenKind where
  display tok = case tok of
    IdentTok    val -> "identifier `" ++ val ++ "`"
    PolyTok     val -> "polymorphic identifier `" ++ val ++ "`"
    StrTok      val -> "string `" ++ val ++ "`"
    IntegerTok  val -> "integer `" ++ show val ++ "`"
    FloatTok    val -> "float `" ++ show val ++ "`"
    OperatorTok val -> "operator `" ++ val ++ "`"
    EofTok          -> "end of file"

    _               -> "token `" ++ getStaticTok tok ++ "`"
