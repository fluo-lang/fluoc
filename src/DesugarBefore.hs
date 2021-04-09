module DesugarBefore where

import           Data.Generics.Uniplate.Operations
import           Syntax.Ast
import           Errors.Diagnostics
import           Sources
import           Syntax.Rewrite                 ( fnAppName )
import           Text.Printf                    ( printf )

desugarBefore :: [Statement] -> Failable [Statement]
desugarBefore ss = do
  let filtered = filter isRemove ss
  filtered' <- rewriteBiM rewriteOp filtered
  transformBiM rewriteStmt filtered'

isRemove :: Statement -> Bool
isRemove s = case s of
  OpDefS{} -> False
  _        -> True

addFixivity :: String -> String -> String
addFixivity = printf "`%s` (%s)"

convertOperator :: Fixivity -> Operator -> Ident
convertOperator fix (Operator op sp) = Ident converted sp
  where converted = addFixivity op (show fix)

rewriteStmt :: Statement -> Failable Statement
rewriteStmt (DeclarationS (Declaration (OpId fix s) expr s') s'') =
  return $ DeclarationS (Declaration (convertOperator fix s) expr s') s''
rewriteStmt (BindingS bs span') = return $ BindingS (mapBinding <$> bs) span'
 where
  mapBinding (Binding (Just (OpId fix s)) ps expr s') =
    Binding (Just (convertOperator fix s)) ps expr s'
  mapBinding b = b
rewriteStmt s = return s

rewriteOp :: Expr -> Failable (Maybe Expr)
rewriteOp (OpE oped s) = return . Just $ re oped
 where
  re (BinOp (Operator op _) a b) | op == fnAppName = FnAppE a b s
  re (BinOp o a b) = FnAppE
    ( FnAppE (VariableE (id2ns $ convertOperator BinaryF o) $ getSpan o) a
    $ bt a o
    )
    b
    s
  re (PreOp o a) =
    FnAppE (VariableE (id2ns $ convertOperator PrefixF o) $ getSpan o) a
      $ bt a o
  re (PostOp o a) =
    FnAppE (VariableE (id2ns $ convertOperator PostfixF o) $ getSpan o) a
      $ bt a o
rewriteOp _ = return Nothing
