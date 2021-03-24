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
    elif         { MkToken _ ElifTok }
    match        { MkToken _ MatchTok }
    '"'          { MkToken _ DoubleQouteTok }
    "'"          { MkToken _ SingleQouteTok }
    '('          { MkToken _ LParenTok }
    ')'          { MkToken _ RParenTok }
    '['          { MkToken _ LBracketTok }
    ']'          { MkToken _ RBracketTok }
    '{'          { MkToken _ LCurlyTok }
    '}'          { MkToken _ RCurlyTok }
    '_'          { MkToken _ (IdentTok "_") }
    break        { MkToken _ BreakTok }
    identifier   { MkToken _ (IdentTok _) }
    string       { MkToken _ (StrTok _) }
    integer      { MkToken _ (IntegerTok _) }
    float        { MkToken _ (FloatTok _) }
    ','          { MkToken _ (OperatorTok ",") }
    '::'         { MkToken _ (OperatorTok "::") }
    ':'          { MkToken _ (OperatorTok ":") }
    '='          { MkToken _ (OperatorTok "=") }
    operator     { MkToken _ (OperatorTok _) }

%right in let if
%nonassoc string float integer '(' '_' identifier
%nonassoc VARIANT
%nonassoc OPPAT
%left operator
%nonassoc OPEXPR
%nonassoc FUNAPP
%left '->' TYPEOP
%nonassoc TYPEAPP

%%

Statements      : StatementsInner            { $1 }
                | {- empty -}                { [] }
StatementsInner : Statements break Statement { $3 : $1 }
                | Statement                  { [$1] }

Statement : DecStatement { $1 }
          | LetStatement { $1 }

LetStatement : let Ident ':' Patterns '=' Expr { let pos = bt $1 $6
                                               in BindingS $2 (Binding $4 $6 pos) pos}
DecStatement : dec Ident ':' Type              { let pos = bt $1 $4
                                               in DeclarationS (Declaration $2 $4 pos) pos}

Expr : Literal                                         { LiteralE $1 (getSpan $1)}
     | Expr Operator Expr %prec OPEXPR                 { BinOpE $1 $2 $3 (bt $1 $3) }
     | Tuple                                           { $1 }
     | Namespace                                       { VariableE $1 $ getSpan $1 }
     | let Bindings in '{' Expr '}'                    { LetInE $2 $5 $ bt $1 $6 }
     | if Expr '{' Expr '}' else '{' Expr '}'          { CondE ($2, $4) [] $8 $ bt $1 $9}
     | if Expr '{' Expr '}' ElifCond else '{' Expr '}' { CondE ($2, $4) $6 $9 $ bt $1 $10}
     | Expr Expr %prec FUNAPP                          { BinOpE $1 
                                                           (Operator "application" $
                                                               gap (getSpan $1) (getSpan $2))
                                                           $2
                                                           (bt $1 $2) }

Bindings      : BindingsInner             { reverse $1 }
BindingsInner : BindingsInner ',' Binding { ($3:$1) }
              | Binding                   { [$1] }
Binding       : Patterns '=' Expr         { Binding $1 $3 (bt (head $1) $3)}

Patterns      : PatternsInner                { reverse $1 }
PatternsInner : PatternsInner PatternBinding { ($2:$1) }
              | PatternBinding               { [$1] }

PatternBinding        : Ident                           { BindP $1 (getSpan $1) }
                      | '(' Pattern ')'                 { setSpan (bt $1 $3) $2 }
Pattern               : Ident                           { BindP $1 (getSpan $1) }
                      | Namespace Pattern %prec VARIANT { VariantP $1 $2 (bt $1 $2) }
                      | Pattern Operator Pattern %prec OPPAT { CustomP $1 $2 $3 (bt $1 $3) }
                      | '(' Pattern ')'                 { setSpan (bt $1 $3) $2 }

ElifCond      : ElifCondInner          { reverse $1 }
ElifCondInner : ElifCondInner Elif     { ($2:$1) }
              | Elif                   { [$1] }
Elif          : elif Expr '{' Expr '}' { ($2, $4) }

Tuple     : '(' ')'               { TupleE [] (bt $1 $2) }
          | '(' ',' ')'           { TupleE [] (bt $1 $3) }
          | '(' Expr ',' ')'      { TupleE [$2] (bt $1 $4)}
          | '(' TupleExpr ',' ')' { let reved = reverse $2 in TupleE reved (bt $1 $4)}
          | '(' TupleExpr ')'     { let reved = reverse $2 in TupleE reved (bt $1 $3)}
          | '(' Expr ')'          { GroupedE $2 (bt $1 $3) }
TupleExpr : TupleExpr ',' Expr    { ($3:$1) }
          | Expr ',' Expr         { [$3, $1] }

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
              | Type Type  %prec TYPEAPP   { BinOpType $1 (Operator "application" $ gap (getSpan $1) (getSpan $2)) $2 (bt $1 $2)}
              | Type Operator Type %prec TYPEOP { BinOpType $1 $2 $3 (bt $1 $3) }
TupleType     : TupleType ',' Type         { ($3:$1) }
              | Type ',' Type              { [$3, $1] }

Ident          : identifier                { case $1 of (MkToken span (IdentTok s)) -> Ident s span}
Namespace      : NamespaceInner            { let reved = reverse $1 
                                              in Namespace reved (bt (head reved) (head $1)) }
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
  (case expects of
    [] -> Nothing
    [x] -> Just $ "expected " ++ x
    _ -> (Just $ "expected " ++ (intercalate ", " (init expects)) ++ ", or " ++ (last expects)))

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
