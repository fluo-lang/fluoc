module Errors.Diagnostics where

import           Sources                        ( Span )
import           Control.Monad.Except           ( Except )

getLineColStr :: String -> Int -> (Int, Int) -> (Int, Int)
getLineColStr _        0 res    = res
getLineColStr []       _ res    = res
getLineColStr (x : xs) i (l, c) = case x of
  '\n' -> getLineColStr xs (i - 1) (l + 1, 0)
  _    -> getLineColStr xs (i - 1) (l, c + 1)

data DiagnosticType = Error | Warning | Info deriving (Eq)
instance Show DiagnosticType where
  show Error   = "error"
  show Warning = "warning"
  show Info    = "info"

dTyPriority :: DiagnosticType -> Int
dTyPriority Error   = 2
dTyPriority Warning = 1
dTyPriority Info    = 0

data DiagnosticKind = UnexpectedCharacterError | SyntaxError | UnboundVariableError
  deriving (Eq)

getId :: DiagnosticKind -> Int
getId e = case e of
  UnexpectedCharacterError -> 0
  SyntaxError              -> 1
  UnboundVariableError     -> 2

instance Show DiagnosticKind where
  show e = case e of
    UnexpectedCharacterError -> "unexpected character error"
    SyntaxError              -> "syntax error"
    UnboundVariableError     -> "unbound variable error"

data Annotation = Annotation
  { aSpan    :: Span
  , aMessage :: Maybe String
  , aTy      :: DiagnosticType
  }
  deriving (Show, Eq)

data Diagnostic = Diagnostic
  { dTy          :: DiagnosticType
  , dKind        :: DiagnosticKind
  , dAnnotations :: [Annotation]
  , dSpan        :: Span
  , dNote        :: [String]
  }
  deriving (Show, Eq)

newtype Diagnostics = Diagnostics [Diagnostic] deriving (Show, Eq)

intoDiagnostics :: Diagnostic -> Diagnostics
intoDiagnostics d = Diagnostics [d]

type Failable a = Except Diagnostics a
