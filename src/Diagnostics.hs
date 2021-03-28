module Diagnostics where

import           Sources                        ( Span )
import           Control.Monad.State            ( liftIO )
import           Compiler

getLineColStr :: String -> Int -> (Int, Int) -> (Int, Int)
getLineColStr _        0 res    = res
getLineColStr []       _ res    = res
getLineColStr (x : xs) i (l, c) = case x of
  '\n' -> getLineColStr xs (i - 1) (l + 1, 0)
  _    -> getLineColStr xs (i - 1) (l, c + 1)

data DiagnosticType = Error | Warning | Info deriving (Show, Eq)
data DiagnosticKind = UnexpectedCharacterError | SyntaxError
  deriving (Eq)

getId :: DiagnosticKind -> Int
getId e = case e of
  UnexpectedCharacterError -> 0
  SyntaxError              -> 1

instance Show DiagnosticKind where
  show e =
    show (getId e)
      ++ (case e of
           UnexpectedCharacterError -> "unexpected character"
           SyntaxError              -> "syntax error"
         )

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
  , note         :: Maybe String
  }
  deriving (Show, Eq)

newtype Diagnostics = Diagnostics [Diagnostic] deriving (Show, Eq)

intoDiagnostics :: Diagnostic -> Diagnostics
intoDiagnostics d = Diagnostics [d]

type Failable a = Either Diagnostics a

report :: Diagnostics -> Compiler ()
report d = liftIO $ print d
