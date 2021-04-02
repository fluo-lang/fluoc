{
{-# LANGUAGE FlexibleContexts #-}
module Syntax.Parser where

import           Syntax.Ast
import           Syntax.Token
import           Syntax.Lexer

import           Errors.Diagnostics
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
    "let"                  { MkToken _ LetTok }
    "rec"                  { MkToken _ RecTok }
    "impl"                 { MkToken _ ImplTok }
    "trait"                { MkToken _ TraitTok }
    "dec"                  { MkToken _ DecTok }
    "in"                   { MkToken _ InTok }
    "if"                   { MkToken _ IfTok }
    "else"                 { MkToken _ ElseTok }
    "elif"                 { MkToken _ ElifTok }
    "match"                { MkToken _ MatchTok }
    "assign"               { MkToken _ AssignTok }
    "opdef"                { MkToken _ OpDefTok }

    "left"                 { MkToken _ (IdentTok "left") }
    "right"                { MkToken _ (IdentTok "right") }
    "nonassoc"             { MkToken _ (IdentTok "nonassoc") }
    "prefix"               { MkToken _ (IdentTok "prefix") }
    "postfix"              { MkToken _ (IdentTok "postfix") }
    "binary"               { MkToken _ (IdentTok "binary") }

    '('                    { MkToken _ LParenTok }
    ')'                    { MkToken _ RParenTok }
    '['                    { MkToken _ LBracketTok }
    ']'                    { MkToken _ RBracketTok }
    '{'                    { MkToken _ LCurlyTok }
    '}'                    { MkToken _ RCurlyTok }
    identifier             { MkToken _ (IdentTok _) }
    string                 { MkToken _ (StrTok _) }
    integer                { MkToken _ (IntegerTok _) }
    float                  { MkToken _ (FloatTok _) }
    ','                    { MkToken _ (OperatorTok ",") }
    backslash              { MkToken _ (OperatorTok "\\") }
    '::'                   { MkToken _ (OperatorTok "::") }
    '|'                    { MkToken _ (OperatorTok "|") }
    ':'                    { MkToken _ (OperatorTok ":") }
    '='                    { MkToken _ (OperatorTok "=") }
    '=>'                   { MkToken _ (OperatorTok "=>") }
    operator               { MkToken _ (OperatorTok _) }
    polymorphic_identifier { MkToken _ (PolyTok _) }
    end_of_file            { MkToken _ EofTok }

%right "in" "let" "if" "assign" "match" backslash "impl" "trait" "rec" "dec" "opdef"
%nonassoc PREOP POSTOP
%left '=>' TYPEOP
%nonassoc string float integer '(' identifier polymorphic_identifier "left" "right" "nonassoc" "prefix" "postfix" "binary"
%nonassoc VARIANT
%nonassoc OPPAT
%left operator
%nonassoc OPEXPR
%nonassoc FUNAPP
%nonassoc TYPEAPP
%nonassoc EOF

%%

Statements      : StatementsInner end_of_file { reverse $1 }
                | end_of_file %prec EOF       { [] }
StatementsInner : StatementsInner Statement   { $2 : $1 }
                | Statement                   { [$1] }

Statement : DecStatement  { $1 }
          | LetStatement  { $1 }
          | RecordDec     { $1 }
          | TraitDec      { $1 }
          | ImplStatement { $1 }
          | OpDef         { $1 }

OpDef         : "opdef"'(' Operator ')' Associativity Fixity Integer
                  { let (prec, span) = $7
                     in OpDefS $3 (OpInfo $5 $6 prec) $ btwn (getSpan $1) span }
Associativity : "left"     { LeftA }
              | "right"    { RightA }
              | "nonassoc" { Nonassoc }
Fixity        : "prefix"   { Prefix }
              | "postfix"  { Postfix }
              | "binary"   { Binary }

ImplStatement : "impl" Namespace ':' Type '{' Statements '}' { ImplS $2 $4 $6 $ bt $1 $7 }
TraitDec      : "trait" Ident ':' PolyIdents '{' Statements '}' { TraitS $2 $4 $6 $ bt $1 $7 }

RecordDec         : "rec" Ident ':' PolyIdents '=' RecordItems { RecordS $2 $4 $6 $ bt $1 $ last $6 }
                  | "rec" Ident '=' RecordItems                { RecordS $2 [] $4 $ bt $1 $ last $4 }
RecordItems       : RecordItemsInner                { reverse $1 }
RecordItemsInner  : RecordItemsInner '|' RecordItem { ($3:$1) }
                  | RecordItem                      { [$1] }
RecordItem        : Ident Types                     { Product $1 $2 $ bt $1 $ last $2 }
                  | Ident '{' Declarations '}'      { NamedProduct $1 $3 $ bt $1 $4 }
PolyIdents        : PolyIdentsInner                 { reverse $1 }
PolyIdentsInner   : PolyIdentsInner PolyIdent       { ($2:$1) }
                  | PolyIdent                       { [$1] }
Types             : TypesInner                      { reverse $1 }
TypesInner        : TypesInner TypeLimited          { ($2:$1) }
                  | TypeLimited                     { [$1] }
Declarations      : DeclarationsInner               { reverse $1 }
DeclarationsInner : DeclarationsInner Declaration   { ($2:$1) }
                  | Declaration                     { [$1] }

LetStatement : "let" Bindings      { BindingS $2 $ bt $1 $ last $2 }

DecStatement : Declaration         { DeclarationS $1 $ getSpan $1 }
Declaration : "dec" Ident ':' Type { Declaration $2 $4 $ bt $1 $4 }

Expr      : ExprInner                                           { case $1 of
                                                                        [OtherTok t] -> t
                                                                        _ -> let reved = reverse $1 
                                                                              in ExprList reved $ bt (head reved) (head $1)
                                                                }
ExprInner : ExprInner OpTok                                     { ($2:$1) }
          | OpTok                                               { [$1] }
OpTok     : ExprItem                                            { OtherTok $1 }
          | Operator                                            { OpTok $1 }
ExprItem  : Literal                                             { LiteralE $1 $ getSpan $1}
          | Tuple                                               { $1 }
          | Namespace                                           { VariableE $1 $ getSpan $1 }
          | "assign" Bindings "in" '{' Expr '}'                 { LetInE $2 $5 $ bt $1 $6 }
          | "match" Expr '{' MatchBranches '}'                  { MatchE $2 $4 $ bt $1 $5 }
          | "if" Expr '{' Expr '}' "else" '{' Expr '}'          { CondE ($2, $4) [] $8 $ bt $1 $9 }
          | "if" Expr '{' Expr '}' ElifCond "else" '{' Expr '}' { CondE ($2, $4) $6 $9 $ bt $1 $10 }
          | backslash Patterns '=>' Expr                        { LambdaE $2 $4 $ bt $1 $4 }
          | '(' Expr ')'                                        { $2 }

MatchBranches      : MatchBranchesInner                 { reverse $1 }
MatchBranchesInner : MatchBranchesInner ',' MatchBranch { ($3:$1) }
                   | MatchBranch                        { [$1] }
MatchBranch        : Expr '=>' Expr                     { MatchBranch $1 $3 $ bt $1 $3 }

Bindings      : BindingsInner               { reverse $1 }
BindingsInner : BindingsInner ',' Binding   { ($3:$1) }
              | Binding                     { [$1] }
Binding       : Ident ':' Patterns '=' Expr { Binding (Just $1) $3 $5 $ bt $1 $5 }
              | Expr '=' Expr               { Binding Nothing [$1] $3 $ bt $1 $3 }

Patterns      : PatternsInner                { reverse $1 }
PatternsInner : PatternsInner PatternBinding { ($2:$1) }
              | PatternBinding               { [$1] }

PatternBinding        : Namespace            { VariableE $1 $ getSpan $1 }
                      | '(' Expr ')'         { $2 }

ElifCond      : ElifCondInner          { reverse $1 }
ElifCondInner : ElifCondInner Elif     { ($2:$1) }
              | Elif                   { [$1] }
Elif          : "elif" Expr '{' Expr '}' { ($2, $4) }

Tuple     : '(' ')'               { TupleE [] (bt $1 $2) }
          | '(' ',' ')'           { TupleE [] (bt $1 $3) }
          | '(' Expr ',' ')'      { TupleE [$2] (bt $1 $4)}
          | '(' TupleExpr ',' ')' { let reved = reverse $2 in TupleE reved (bt $1 $4)}
          | '(' TupleExpr ')'     { let reved = reverse $2 in TupleE reved (bt $1 $3)}
TupleExpr : TupleExpr ',' Expr    { ($3:$1) }
          | Expr ',' Expr         { [$3, $1] }

Operator : operator {case $1 of (MkToken span (OperatorTok o)) -> Operator o span}

Literal : string   { case $1 of (MkToken span (StrTok s)) -> StringL s span}
        | float    { case $1 of (MkToken span (FloatTok f)) -> FloatL f span}
        | integer  { case $1 of (MkToken span (IntegerTok i)) -> IntegerL i span}

Integer : integer  { case $1 of (MkToken span (IntegerTok i)) -> (i, span)}

PolyIdent : polymorphic_identifier {case $1 of (MkToken span (PolyTok p)) -> PolyIdent p span}

TypeLimited   : Namespace                          { NamespaceType $1 $ getSpan $1 }
              | '(' TupleType ')'                  { let reved = reverse $2 in TupleType reved (bt $1 $3)}
              | '(' ')'                            { TupleType [] $ bt $1 $2}
              | '(' ',' ')'                        { TupleType [] $ bt $1 $3}
              | '(' Type ',' ')'                   { TupleType [$2] $ bt $1 $4}
              | '(' TupleType ',' ')'              { let reved = reverse $2 in TupleType reved (bt $1 $4)}
              | '(' Type ')'                       { $2 }
              | PolyIdent                          { PolyType $1 $ getSpan $1 }
Type          : TypeInner                          { case $1 of
                                                       [OtherTok t] -> t
                                                       _ -> let reved = reverse $1 
                                                             in TypeList reved $ bt (head reved) (head $1)
                                                   }
TypeInner     : TypeInner OpTokTy                  { ($2:$1) }
              | OpTokTy                            { [$1] }
OpTokTy       : Operator                           { OpTok $1 }
              | TypeLimited                        { OtherTok $1 }

TupleType     : TupleType ',' Type              { ($3:$1) }
              | Type ',' Type                   { [$3, $1] }

Ident          : identifier                { mkIdent $1 }
               | '(' Operator ')'          { OpId $2 }
               | "left"                    { mkIdent $1 }
               | "right"                   { mkIdent $1 }
               | "prefix"                  { mkIdent $1 }
               | "postfix"                 { mkIdent $1 }
               | "binary"                  { mkIdent $1 }
               | "nonassoc"                { mkIdent $1 }

Namespace      : NamespaceInner            { let reved = reverse $1 
                                              in Namespace reved (bt (head reved) (head $1)) }
NamespaceInner : Ident                     { [$1] }
               | NamespaceInner '::' Ident { ($3:$1) }

{

mkIdent :: Token -> Ident
mkIdent tok = case tok of (MkToken span (IdentTok s)) -> Ident s span

-- Calculate Span
cs :: Token -> Token -> Span
cs (MkToken s _) (MkToken e _) = btwn s e

syntaxErr expects str span = Diagnostic
  Error
  SyntaxError
  [Annotation span (Just $ "Unexpected " ++ str) Error]
  span
  (case expects of
    [] -> []
    [x] -> ["expected " ++ x]
    _ -> ["expected " ++ (intercalate ", " (init expects)) ++ ", or " ++ (last expects)])

parseError :: ([Token], [String]) -> Except Diagnostic a
parseError (((MkToken span t) : ts), expects) =
  throwError $ syntaxErr expects (display t) span
parseError ([], expects) = error "there should be at least an eof token"

parseBlock :: SourceId -> String -> Either Diagnostic [Statement]
parseBlock sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  statements tokenStream

parseExpr :: SourceId -> String -> Either Diagnostic Expr
parseExpr sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  expr $ init tokenStream

parseType :: SourceId -> String -> Either Diagnostic Type
parseType sourceId input = runExcept $ do
  tokenStream <- scanTokens sourceId input
  ty $ init tokenStream

parseTokens :: SourceId -> String -> Either Diagnostic [Token]
parseTokens sourceId source = runExcept $ scanTokens sourceId source
}
