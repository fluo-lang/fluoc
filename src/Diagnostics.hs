module Diagnostics where

import           Sources                        ( Span )
import           Control.Monad.State            ( liftIO )
import           Compiler

data DiagnosticType = Error | Warning | Info deriving (Show, Eq)
data DiagnosticKind = UnexpectedCharacterError | SyntaxError
  deriving (Show, Eq)

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
