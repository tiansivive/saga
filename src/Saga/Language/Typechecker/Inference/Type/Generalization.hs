{-# LANGUAGE MonoLocalBinds #-}
module Saga.Language.Typechecker.Inference.Type.Generalization where

import           Data.List                                       (nub)
import qualified Data.Map                                        as Map
import           Effectful                                       (Eff)
import qualified Effectful.Writer.Static.Local                   as Eff
import           Saga.Language.Core.Literals
import           Saga.Language.Typechecker.Inference.Inference
import qualified Saga.Language.Typechecker.Inference.Type.Shared as Shared
import           Saga.Language.Typechecker.Protocols             (ProtocolID)
import qualified Saga.Language.Typechecker.Qualification         as Q
import           Saga.Language.Typechecker.Qualification         (Given (..),
                                                                  Qualified (..))
import qualified Saga.Language.Typechecker.Solver.Constraints    as CST
import           Saga.Language.Typechecker.Solver.Constraints    (Constraint (..))
import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (Scheme (..),
                                                                  Type)

instance Generalize Type where
  generalize (T.Tuple tys) = do
    ts <- mapM generalize tys
    let (bs, cs, tvars, ts') = foldl accumulate (Map.empty, [], [], []) ts
    return $ Forall tvars (bs :| cs :=> T.Tuple ts')
    where
      accumulate (bs, cs, tvars, ts) (Forall tvars' (bs' :| cs' :=> t)) = (bs <> bs', nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ ts ++ [t])
  generalize (T.Record pairs) = do
    qPairs <- mapM (mapM generalize) pairs
    let (bs, cs, tvars, pairs') = foldl accumulate (Map.empty, [], [], []) qPairs
    return $ Forall tvars (bs :| cs :=> T.Record pairs')
    where
      accumulate (bs, cs, tvars, pairs) (key, Forall tvars' (bs' :| cs' :=> t)) = (bs <> bs', nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ pairs ++ [(key, t)])

  generalize (T.Arrow arg out) = do
    Forall tvars (cs :=> arg') <- generalize arg
    return $ Forall tvars (cs :=> arg' `T.Arrow` out)

  generalize (T.Union tys) = do
    tys' <- mapM generalize tys
    let (tvars, bs, cs, qts) = mapM (\(Forall tvars (bs :| cs :=> qt)) -> (tvars, bs, cs, qt)) tys'
    return $ Forall (nub tvars) (bs :| nub cs :=> T.Union (nub qts))
  generalize ty = case ty of
    T.Singleton lit -> generalize' $ case lit of
      LString _ -> "IsString"
      LBool _   -> "IsBool"
      LInt _    -> "Num"
    T.Data prim _ -> generalize' $ case prim of
      "Int"    -> "Num"
      "String" -> "IsString"
      "Bool"   -> "IsBool"
    _ -> return $ Forall [] (Q.none :=> ty)

    where
      generalize' :: Shared.TypeInference es => ProtocolID -> Eff es (T.Polymorphic Type)
      generalize' protocol = do
        tvar <- Shared.fresh U
        e <- Shared.fresh E
        Eff.tell $ Impl e (CST.Mono $ T.Var tvar) protocol
        return $ Forall [tvar] (Map.empty :| [T.Var tvar `Q.Implements` protocol] :=> T.Var tvar)

