module Typecheck.Annotation where

import           Syntax.Ast                     ( Namespace
                                                , Ident
                                                , PolyIdent
                                                , Literal
                                                )
import           Typecheck.Type
import           Sources

data TPattern = ConsP Namespace [TPattern] Span

data TBinding = TBind (Either Ident TPattern) TExpr Span
data TDeclaration = TDeclaration Ident Type Span
data TRecordItem = TProduct Ident [Type] Span
                 | TNamedProduct Ident [TDeclaration] Span

data TyStatement = TBindingS TBinding Span
                 | TDeclarationS TDeclaration Span
                 | TRecordS Ident [PolyIdent] [TRecordItem]

data TMatchBranch = TMatchBranch TPattern TExpr Span

data TExpr = LiteralE Literal Type Span
           | CondE (TExpr, TExpr) [(TExpr, TExpr)] TExpr Type Span
           | LetInE [TBinding] TExpr Type Span
           | VariableE Namespace Type Span
           | LambdaE [TPattern] TExpr Type Span
           | MatchE TExpr [TMatchBranch] Type Span
           | FnAppE TExpr TExpr Type Span
