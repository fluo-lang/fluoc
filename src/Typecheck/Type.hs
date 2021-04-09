module Typecheck.Type where
import           Syntax.Ast                     ( Namespace )

newtype TVar = TV Int
  deriving (Eq, Show, Ord)

data Type = TVar TVar
          | TCons Namespace [Type]
          deriving (Eq, Show)
