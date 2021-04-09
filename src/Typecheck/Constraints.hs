{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Typecheck.Constraints where

import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , throwError
                                                )
import           Control.Monad.State            ( evalStateT
                                                , StateT
                                                , get
                                                , put
                                                )
import           Control.Monad.Reader           ( runReaderT
                                                , local
                                                , ReaderT
                                                , ask
                                                , lift
                                                )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Text.Printf                    ( printf )

import           Sources
import           Typecheck.Type
import           Errors.Diagnostics
import           Syntax.Ast                     ( Namespace(..) )
import           Display

newtype InferState = ISt Int
initInfer :: InferState
initInfer = ISt 0

newtype Constraint = Constr (Type, Type) deriving (Show, Eq)
instance Substitutable Constraint where
  apply subst (Constr tys) = Constr $ apply subst <$> tys
  ftv (Constr (t1, t2)) = ftv t1 `S.union` ftv t2

type InferM a = ReaderT Context (StateT InferState (Except Diagnostics)) a
runInfer
  :: Context
  -> InferM (Type, [Constraint])
  -> Either Diagnostics (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

inEnv :: (Namespace, Scheme) -> InferM a -> InferM a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

lookupEnv :: Namespace -> InferM Type
lookupEnv x = do
  (TContext env) <- ask
  case M.lookup x env of
    Nothing -> throwError $ unboundVariable x
    Just s  -> instantiate s

fresh :: Span -> InferM Type
fresh span' = do
  (ISt old) <- lift get
  put $ ISt $ old + 1
  return $ TVar (TV old span')

instantiate :: Scheme -> InferM Type
instantiate (Forall ts t) = do
  ts' <- mapM (fresh . getSpan) ts
  let s = Substs $ M.fromList $ zip ts ts'
  return $ apply s t

generalize :: Context -> Type -> Scheme
generalize env t  = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv env

unboundVariable :: Namespace -> Diagnostics
unboundVariable ns@(Namespace _ s) = intoDiagnostics $ Diagnostic
  Error
  UnboundVariableError
  [ Annotation s
               (Just (printf "the variable `%s` is undefined" $ display ns))
               Error
  ]
  s
  []
