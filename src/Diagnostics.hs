module Diagnostics where

import           Sources                           ( Span )

data DiagnosticType = Error | Warning | Info;
data DiagnosticKind = SyntaxError

data Annotation = Annotation
  { aSpan    :: Span
  , aMessage :: Maybe String
  , aTy      :: DiagnosticType
  }

data Diagnostic = Diagnostic
  { dTy          :: DiagnosticType
  , dKind        :: DiagnosticKind
  , dAnnotations :: [Annotation]
  , dSpan        :: Span
  }

newtype Diagnostics = Diagnostics [Diagnostic]

type Failable a = Either Diagnostics a
