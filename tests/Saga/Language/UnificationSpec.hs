module Saga.Language.UnificationSpec where

import           Test.Hspec

import           Control.Monad.Except
import           Control.Monad.Reader                               (ReaderT (runReaderT))
import           Debug.Trace
import qualified Data.Set                                      as Set



spec :: Spec
spec = do
  describe "Unification" $ do
    describe "Unions:" $ do
      it "Skipping - unifies 2 unions" $ do
        -- let u1 = TUnion $ Set.fromList [TVar (Tyvar "a" KType), TPrimitive TString, TPrimitive TBool]
        -- let u2 = TUnion $ Set.fromList [TPrimitive TString, TArrow (TPrimitive TString) (TPrimitive TString), TVar (Tyvar "b" KType) ]
        -- Scheme [] ([] :=> HM.TLiteral (HM.LInt n)) <- return $ infer "1"

       -- let sub = runExceptT $ runReaderT (u1 `unify` u2) defaultEnv

        1 `shouldBe` 1
     