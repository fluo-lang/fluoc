module Diagnostics where

import           Sources                        ( Span )

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

type Failable a = Either Diagnostics a
