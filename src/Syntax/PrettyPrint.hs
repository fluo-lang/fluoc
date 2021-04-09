{-# LANGUAGE FlexibleInstances #-}
module Syntax.PrettyPrint where

import           Data.Tree                      ( Tree(..) )
import           Data.List                      ( intercalate )
import           Syntax.Ast

class SS a where
  ss :: a -> String

instance SS Ident where
  ss (Ident s _ ) = "ident: " ++ s
  ss (OpId  _ op) = ss op

instance SS PolyIdent where
  ss (PolyIdent s _) = "polymorphic identifier: " ++ s

instance SS Operator where
  ss (Operator s _) = "operator: " ++ s

instance SS Namespace where
  ss (Namespace ids _) =
    "namespace: " ++ intercalate "." ((\(Ident s _) -> s) <$> ids)

-- Pretty print typeclass
class PP a where
  pp :: a -> Tree String

manyPoly :: [PolyIdent] -> String
manyPoly pids = unwords ((\(PolyIdent s _) -> s) <$> pids)
manyId :: [Ident] -> String
manyId ids = unwords ((\(Ident s _) -> s) <$> ids)

instance PP [Statement] where
  pp s = Node "program" $ pp <$> s

instance PP Statement where
  pp (BindingS     [b] _) = pp b
  pp (BindingS     bs  _) = Node "bindings" $ pp <$> bs
  pp (DeclarationS dec _) = pp dec
  pp (ImplS ns ty sts _ ) = Node ("impl " ++ ss ns) $ pp ty : (pp <$> sts)
  pp (TraitS i pids sts _) =
    Node ("trait: " ++ ss i ++ " " ++ manyPoly pids) $ pp <$> sts
  pp (ImportS ns (Just ident) _) =
    Node ("import " ++ ss ns ++ " as " ++ ss ident) []
  pp (ImportS     ns Nothing _) = Node ("import " ++ ss ns) []
  pp (FromImportS ns Nothing _) = Node ("from " ++ ss ns ++ " import *") []
  pp (FromImportS ns (Just ids) _) =
    Node ("from " ++ ss ns ++ " import " ++ manyId ids) []
  pp (RecordS name pids ritems _) =
    Node ("record dec " ++ ss name ++ " " ++ manyPoly pids) $ pp <$> ritems
  pp (OpDefS op info _) = Node ("opdef: " ++ ss op) [pp info]

instance PP OpInfo where
  pp (Prefix  prec) = Node ("prefix: " ++ show prec) []
  pp (Postfix prec) = Node ("postfix: " ++ show prec) []
  pp (Binary prec asc) =
    Node ("binary: " ++ show prec ++ " assoc: " ++ show asc) []

instance PP RecordItem where
  pp (Product i tys _) = Node ("record product: " ++ ss i) $ pp <$> tys
  pp (NamedProduct i decs _) =
    Node ("named record product: " ++ ss i) $ pp <$> decs

instance PP Declaration where
  pp (Declaration i ty _) = Node ("declaration: " ++ ss i) [pp ty]

instance PP Type where
  pp (Infer _            ) = Node "infer type" []
  pp (Never _            ) = Node "never type" []
  pp (NamespaceType ns  _) = Node ("namespace type: " ++ ss ns) []
  pp (TypeList      ops _) = Node "type list" $ pp <$> ops
  pp (TupleType     tys _) = Node "tuple type" $ pp <$> tys
  pp (OpType        op  _) = pp op
  pp (PolyType      pid _) = Node ("polymorphic type: " ++ ss pid) []
  pp (TyApp a b _    ) = Node "type application" [pp a, pp b]

instance PP Binding where
  pp (Binding (Just b) ps e _) =
    Node ("binding: " ++ ss b) (pp e : (pp <$> ps))
  pp (Binding Nothing ps e _) = Node "binding" ((pp <$> ps) ++ [pp e])

instance PP Expr where
  pp (LiteralE l   _) = pp l
  pp (ExprList ops _) = Node "expr list" $ pp <$> ops
  pp (OpE      op  _) = pp op
  pp (TupleE   es  _) = Node "tuple" (pp <$> es)
  pp (CondE ce ces e _) =
    Node "conditional" $ ppIf ce : (ppElseIf <$> ces) ++ [ppElse e]
  pp (LetInE bs e _   ) = Node "let in" $ (pp <$> bs) ++ [pp e]
  pp (VariableE ns _  ) = Node ("variable: " ++ ss ns) []
  pp (LambdaE ps e   _) = Node "lambda" $ (pp <$> ps) ++ [pp e]
  pp (MatchE  e  mbs _) = Node "match" $ pp e : (pp <$> mbs)
  pp (FnAppE  a  b   _) = Node "fn app" [pp a, pp b]

instance PP MatchBranch where
  pp (MatchBranch p e _) = Node "match branch" [pp p, pp e]

ppIf :: (Expr, Expr) -> Tree String
ppIf (c, e) = Node "if" [pp c, pp e]

ppElseIf :: (Expr, Expr) -> Tree String
ppElseIf (c, e) = Node "else if" [pp c, pp e]

ppElse :: Expr -> Tree String
ppElse e = Node "else" [pp e]

instance PP a => PP (OpTok a) where
  pp (OpTok    op) = Node (ss op) []
  pp (OtherTok a ) = pp a

instance PP a => PP (Oped a) where
  pp (BinOp op a a') = Node ("binary operator: " ++ ss op) [pp a, pp a']
  pp (PreOp  op a  ) = Node ("prefix operator: " ++ ss op) [pp a]
  pp (PostOp op a  ) = Node ("postfix operator: " ++ ss op) [pp a]

instance PP Literal where
  pp (IntegerL i _) = Node ("integer " ++ show i) []
  pp (FloatL   d _) = Node ("float " ++ show d) []
  pp (StringL  s _) = Node ("string " ++ show s) []
