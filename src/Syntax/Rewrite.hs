module Syntax.Rewrite where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Generics.Uniplate.Operations
import           Syntax.Ast
import           Sources
import           Errors.Diagnostics

data OpEntry = OpEntry
  { assoc :: Associativity
  , prec  :: Integer
  } deriving (Eq, Show)
data Rules = Rules
  { prefixOps  :: Map String OpEntry
  , binaryOps  :: Map String OpEntry
  , postfixOps :: Map String OpEntry
  } deriving (Eq, Show)

getOpRules :: [Statement] -> Rules
getOpRules = foldl
  (\(Rules pre bin post) s -> case s of
    OpDefS (Operator op _) (OpInfo asc f p) _ -> case f of
      Binary  -> Rules pre (M.insert op (OpEntry asc p) bin) post
      Prefix  -> Rules (M.insert op (OpEntry asc p) pre) bin post
      Postfix -> Rules pre bin (M.insert op (OpEntry asc p) post)
    _ -> Rules pre bin post
  )
  (Rules M.empty M.empty M.empty)

rewrite :: [Statement] -> Failable [Statement]
rewrite ss = do
  exprRewrite <- rewriteBiM (rewriteExpr rs) ss
  rewriteBiM (rewriteType rs) exprRewrite
  where rs = getOpRules ss

rewriteExpr :: Rules -> Expr -> Failable (Maybe Expr)
rewriteExpr rs (ExprList es _) = Just <$> fixPrec rs es OpE
rewriteExpr _  _               = Right Nothing

rewriteType :: Rules -> Type -> Failable (Maybe Type)
rewriteType rs (TypeList ts _) = Just <$> fixPrec rs ts OpType
rewriteType _  _               = Right Nothing

fixPrec
  :: Rules
  -> OpToks a              -- List of tokens
  -> (Oped a -> Span -> a) -- Construct an `a` from an operator and span
  -> Failable a            -- Produced `a`
fixPrec _ _ _ = undefined
