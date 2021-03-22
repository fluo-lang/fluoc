{
module Syntax.Parser
  ( parseExpr
  , parseTokens
  ) where

import           Syntax.Ast
import           Syntax.Token
import           Syntax.Lexer

import           Diagnostics
import           Sources

import           Control.Monad.Except
import           Data.List                      ( intercalate )
}

-- Entry point
%name expr

-- Entry point
%name expr

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except Diagnostic } { (>>=) } { return }
%errorhandlertype explist
%error { parseError }

-- Token Names
%token
    let          { MkToken _ LetTok }
    rec          { MkToken _ RecTok }
    impl         { MkToken _ ImplTok }
    trait        { MkToken _ TraitTok }
    dec          { MkToken _ DecTok }
    in           { MkToken _ InTok }
    if           { MkToken _ IfTok }
    else         { MkToken _ ElseTok }
    match        { MkToken _ MatchTok }
    '"'          { MkToken _ DoubleQouteTok }
    "'"          { MkToken _ SingleQouteTok }
    '('          { MkToken _ LParenTok }
    ')'          { MkToken _ RParenTok }
    '['          { MkToken _ LBracketTok }
    ']'          { MkToken _ RBracketTok }
    '{'          { MkToken _ LCurlyTok }
    '}'          { MkToken _ RCurlyTok }
    identifier   { MkToken _ (IdentTok $$)}
    string       { MkToken _ (StrTok $$)}
    integer      { MkToken _ (IntegerTok $$)}
    float        { MkToken _ (FloatTok $$)}
    '->'         { MkToken _ (OperatorTok "->")}
    '::'         { MkToken _ (OperatorTok "::")}
    '='          { MkToken _ (OperatorTok "=")}
    operator     { MkToken _ (OperatorTok $$)}

%%

Statement : let { () }

{
-- Calculate Span
cs :: Token -> Token -> Span
cs (MkToken (Span id s _) _) (MkToken (Span _ _ e) _) = Span id s e

syntaxErr expects str span = Diagnostic
  Error
  SyntaxError
  [Annotation span (Just $ "Unexpected " ++ str) Error]
  span
  (Just $ "expected one of: " ++ (intercalate " " expects))

parseError :: ([Token], [String]) -> Except Diagnostic a
parseError (((MkToken span t) : ts), expects) =
  throwError $ syntaxErr expects (show t) span
parseError ([], expects) =
  throwError $ syntaxErr expects "end of file" Eof

parseExpr :: SourceId -> String -> Either Diagnostic ()
parseExpr sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  expr tokenStream

parseTokens :: SourceId -> String -> Either Diagnostic [Token]
parseTokens sourceId source = runExcept $ scanTokens sourceId source
}
