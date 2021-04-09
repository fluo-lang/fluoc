module Typecheck.Type where

import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Syntax.Ast                     ( Namespace )
import           Sources

data TVar = TV Int Span
  deriving Show

instance Ord TVar where
  (TV i _) `compare` (TV i' _) = i `compare` i'
instance Eq TVar where
  (TV i _) == (TV i' _) = i == i'
instance Spanned TVar where
  getSpan (TV _ s) = s

data Type = TVar TVar
          | TCons Namespace [Type] Span
          deriving (Eq, Show)

instance Spanned Type where
  getSpan (TVar tv    ) = getSpan tv
  getSpan (TCons _ _ s) = s

modifySpan :: Span -> Type -> Type
modifySpan s (TVar (TV t _)) = TVar (TV t s)
modifySpan s (TCons ns ts _) = TCons ns ts s

data Scheme = Forall [TVar] Type

newtype Substs = Substs (Map TVar Type)

class Substitutable a where
  apply :: Substs -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply subst (TCons ns tys s) = TCons ns (apply subst <$> tys) s
  apply (Substs m) t@(TVar tv) =
    maybe t (modifySpan $ getSpan tv) $ M.lookup tv m

  ftv (TVar tv      ) = S.singleton tv
  ftv (TCons _ tys _) = foldr (S.union . ftv) S.empty tys

instance Substitutable Scheme where
  apply subst (Forall tv ty) = Forall tv $ apply subst ty
  ftv (Forall tvs ty) = ftv ty `S.difference` S.fromList tvs

-- Typing Context
newtype Context = TContext (Map Namespace Scheme)
extend :: Context -> (Namespace, Scheme) -> Context
extend (TContext m) (ns, scheme) = TContext $ M.insert ns scheme m
remove :: Context -> Namespace -> Context
remove (TContext m) ns = TContext $ M.delete ns m
emptyEnv :: Context
emptyEnv = TContext M.empty

instance Substitutable Context where
  apply subst (TContext ctx) = TContext $ M.map (apply subst) ctx
  ftv (TContext ctx) = foldr (\e acc -> S.union acc (ftv e)) S.empty ctx
