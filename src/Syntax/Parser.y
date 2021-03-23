{
module Syntax.Parser where

import           Syntax.Ast
import           Syntax.Token
import           Syntax.Lexer

import           Diagnostics
import           Sources

import           Control.Monad.Except
import           Data.List                      ( intercalate )
}

-- Entry point
%name statements Statements
%name ty Type
%name expr Expr

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
    '_'          { MkToken _ (IdentTok "_")}
    identifier   { MkToken _ (IdentTok _)}
    string       { MkToken _ (StrTok _)}
    integer      { MkToken _ (IntegerTok _)}
    float        { MkToken _ (FloatTok _)}
    '->'         { MkToken _ (OperatorTok "->")}
    ','          { MkToken _ (OperatorTok ",")}
    '::'         { MkToken _ (OperatorTok "::")}
    ':'          { MkToken _ (OperatorTok ":")}
    '='          { MkToken _ (OperatorTok "=")}
    operator     { MkToken _ (OperatorTok _)}

%nonassoc string float integer '(' '_' identifier
%left operator
%nonassoc FUNAPP
%left '->'
%nonassoc TYPEAPP

%%

Statements      : StatementsInner       { $1 }
                | {- empty -}           { [] }
StatementsInner : Statements Statement  { $2 : $1 }
                | Statement             { [$1] }

Statement : dec Ident ':' Type { let pos = bt $1 $4
                                  in DeclarationS (Declaration $2 $4 pos) pos}

Expr : Literal                { LiteralE $1 (getSpan $1)}
     | Expr Operator Expr     { BinOpE $1 $2 $3 (bt $1 $3) }
     | Expr Expr %prec FUNAPP { FunctionAppE $1 $2 (bt $1 $2) }

Operator : operator {case $1 of (MkToken span (OperatorTok o)) -> Operator o span}

Literal : string   { case $1 of (MkToken span (StrTok s)) -> StringL s span}
        | float    { case $1 of (MkToken span (FloatTok f)) -> FloatL f span}
        | integer  { case $1 of (MkToken span (IntegerTok i)) -> IntegerL i span}

Type          : '_'                        { Infer $ getSpan $1 }
              | Namespace                  { NamespaceType $1 (getSpan $1) }
              | '(' TupleType ')'          { let reved = reverse $2 in TupleType reved (bt $1 $3)}
              | '(' ')'                    { TupleType [] (bt $1 $2)}
              | '(' ',' ')'                { TupleType [] (bt $1 $3)}
              | '(' Type ',' ')'           { TupleType [$2] (bt $1 $4)}
              | '(' TupleType ',' ')'      { let reved = reverse $2 in TupleType reved (bt $1 $4)}
              | '(' Type ')'               { setSpan (bt $1 $3) $2 }
              | Type Type  %prec TYPEAPP   { TypeApplication $1 $2 (bt $1 $2)}
              | Type '->' Type             { FunctionType $1 $3 (bt $1 $3) }
TupleType     : TupleType ',' Type         { ($3:$1) }
              | Type ',' Type              { [$3, $1] }

Ident          : identifier                { case $1 of (MkToken span (IdentTok s)) -> Ident s span}
Namespace      : NamespaceInner            { let reved = reverse $1 
                                              in Namespace reved (bt (head reved) (last reved)) }
NamespaceInner : Ident                     { [$1] }
               | NamespaceInner '::' Ident { ($3:$1) }

{

-- Calculate Span
cs :: Token -> Token -> Span
cs (MkToken s _) (MkToken e _) = btwn s e

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

parseBlock :: SourceId -> String -> Either Diagnostic [Statement]
parseBlock sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  reverse <$> statements tokenStream

parseExpr :: SourceId -> String -> Either Diagnostic Expr
parseExpr sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  expr tokenStream

parseType :: SourceId -> String -> Either Diagnostic Type
parseType sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  ty tokenStream

parseTokens :: SourceId -> String -> Either Diagnostic [Token]
parseTokens sourceId source = runExcept $ scanTokens sourceId source
}
