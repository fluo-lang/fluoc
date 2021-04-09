{-# LANGUAGE TupleSections #-}
module Typecheck.ConstraintsSpec where

import           Test.Hspec                     ( it
                                                , describe
                                                , shouldBe
                                                , Spec
                                                )

import           Typecheck.Constraints
import           Typecheck.Type
import           Errors.Diagnostics
import           Sources

runInferEmpty
  :: InferM (Type, [Constraint]) -> Either Diagnostics (Type, [Constraint])
runInferEmpty = runInfer emptyEnv

sid :: SourceId
sid = SourceId 0
sn :: Int -> Int -> Span
sn = Span sid
d :: Span
d = sn 0 1

spec :: Spec
spec = do
  describe "Typecheck.Constraints.fresh" $ do
    it "should generate fresh variables"
      $          runInferEmpty
                   (do
                     _  <- fresh d
                     _  <- fresh d
                     ty <- fresh d
                     return (ty, [])
                   )
      `shouldBe` Right (TVar (TV 2 d), [])
