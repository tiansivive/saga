module Saga.Language.Typechecker.Inference.Type.Generalization where

import           Data.List                                       (nub)
import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Inference.Inference
import qualified Saga.Language.Typechecker.Inference.Type.Shared as Shared
import           Saga.Language.Typechecker.Protocols             (ProtocolID)
import qualified Saga.Language.Typechecker.Qualification         as Q
import           Saga.Language.Typechecker.Qualification         (Qualified (..))
import qualified Saga.Language.Typechecker.Solver.Constraints    as CST
import           Saga.Language.Typechecker.Solver.Constraints    (Constraint (..))
import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (Scheme (..),
                                                                  Type)

instance Generalize Type where
  generalize (T.Tuple tys) = do
    ts <- mapM generalize tys
    let (cs, tvars, ts') = foldl accumulate ([], [], []) ts
    return $ Forall tvars (cs :=> T.Tuple ts')
    where
      accumulate (cs, tvars, ts) (Forall tvars' (cs' :=> t)) = (nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ ts ++ [t])
  generalize (T.Record pairs) = do
    qPairs <- mapM (mapM generalize) pairs
    let (cs, tvars, pairs') = foldl accumulate ([], [], []) qPairs
    return $ Forall tvars (cs :=> T.Record pairs')
    where
      accumulate (cs, tvars, pairs) (key, Forall tvars' (cs' :=> t)) = (nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ pairs ++ [(key, t)])

  generalize (T.Arrow arg out) = do
    Forall tvars (cs :=> arg') <- generalize arg
    return $ Forall tvars (cs :=> arg' `T.Arrow` out)

  generalize (T.Union tys) = do
    tys' <- mapM generalize tys
    let (tvars, cs, qts) = mapM (\(Forall tvars (cs :=> qt)) -> (tvars, cs, qt)) tys'
    return $ Forall (nub tvars) (nub cs :=> T.Union (nub qts))
  generalize ty = case ty of
    T.Singleton lit -> generalize' $ case lit of
      LString _ -> "IsString"
      LBool _   -> "IsBool"
      LInt _    -> "Num"
    T.Data prim _ -> generalize' $ case prim of
      "Int"    -> "Num"
      "String" -> "IsString"
      "Bool"   -> "IsBool"
    _ -> return $ Forall [] ([] :=> ty)

    where
      generalize' ::  ProtocolID -> Shared.TypeInference (T.Polymorphic Type)
      generalize' protocol = do
        tvar <- Shared.fresh T
        e <- Shared.fresh E
        emit' $ Impl e (CST.Mono $ T.Var tvar) protocol
        return $ Forall [tvar] ([T.Var tvar `Q.Implements` protocol] :=> T.Var tvar)

