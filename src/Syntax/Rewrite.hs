module Syntax.Rewrite where

import           Data.Map                       ( Map )
-- import qualified Data.Map                      as M
import           Syntax.Ast

type Rules = Map Operator OpInfo

rewrite :: [Statement] -> [Statement]
rewrite ss = rewriteStmt <$> ss

rewriteStmt :: Statement -> Statement
rewriteStmt stmt = stmt

rewriteExpr :: Expr -> Expr
rewriteExpr expr = expr
