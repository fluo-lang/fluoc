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
  rewrittenExprs <- rewriteBiM rewriteExprsOp filtered
  rewrittenTypes <- rewriteBiM rewriteTypesOp rewrittenExprs
  transformBiM rewriteStmt rewrittenTypes

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

rewriteExprsOp :: Expr -> Failable (Maybe Expr)
rewriteExprsOp (OpE oped s) = Just <$> rewriteOp s oped FnAppE VariableE
rewriteExprsOp _            = return Nothing

rewriteTypesOp :: Type -> Failable (Maybe Type)
rewriteTypesOp (OpType oped s) = Just <$> rewriteOp s oped TyApp NamespaceType
rewriteTypesOp _               = return Nothing

type AppCons a = (a -> a -> Span -> a)
type NamespaceCons a = (Namespace -> Span -> a)

rewriteOp
  :: Spanned a => Span -> Oped a -> AppCons a -> NamespaceCons a -> Failable a
rewriteOp s (BinOp (Operator op _) a b) appC _ | op == fnAppName =
  return $ appC a b s
rewriteOp s (BinOp o a b) appC nsC = return $ appC
  (appC (nsC (id2ns $ convertOperator BinaryF o) $ getSpan o) a $ bt a o)
  b
  s
rewriteOp _ (PreOp o a) appC nsC =
  return $ appC (nsC (id2ns $ convertOperator PrefixF o) $ getSpan o) a $ bt a o
rewriteOp _ (PostOp o a) appC nsC =
  return $ appC (nsC (id2ns $ convertOperator PostfixF o) $ getSpan o) a $ bt
    a
    o
