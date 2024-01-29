{-# LANGUAGE ViewPatterns #-}
module Saga.Language.Typechecker.Elaboration.Generalization where


import qualified Data.Map                                          as Map
import           Effectful                                         (Eff, (:>))
import qualified Effectful.State.Static.Local                      as Eff
import qualified Saga.Language.Syntax.Elaborated.AST               as AST
import qualified Saga.Language.Syntax.Elaborated.Types             as T
import           Saga.Language.Syntax.Elaborated.Types             (Type)
import           Saga.Language.Syntax.Literals
import qualified Saga.Language.Syntax.Polymorphism                 as Q
import           Saga.Language.Syntax.Polymorphism                 (Given (..),
                                                                    Polymorphic (..),
                                                                    Qualified (..))
import           Saga.Language.Typechecker.Elaboration.Monad       (Generalize (..))
import           Saga.Language.Typechecker.Protocols               (ProtocolID)

import           Control.Monad                                     (forM)
import           Data.List                                         (nub)
import           Debug.Pretty.Simple                               (pTrace)
import qualified Saga.Language.Syntax.Elaborated.Kinds             as K
import qualified Saga.Language.Typechecker.Elaboration.Annotations as Ann
import           Saga.Language.Typechecker.Variables               (Variable)
import           Saga.Utils.Common                                 (fmap2,
                                                                    forM2)
import           Saga.Utils.Operators                              ((<$$>),
                                                                    (|>))


instance Generalize Type where
  generalize e | pTrace ("Generalize:\n" ++ show e) False = undefined
  generalize (T.Tuple tys) = do
    tys <- forM tys (AST.node |> generalize)
    let (bs, cs, tvars) = foldl accumulate (Map.empty, [], []) (fmap T.Polymorphic tys)
    return $ Forall tvars (T.Qualified $ bs :| cs :=> T.Tuple (Ann.decorate . T.Polymorphic <$> tys))


  generalize (T.Record pairs) = do
    qPairs <- forM2 pairs $ AST.node |> generalize
    let (bs, cs, tvars) = foldl accumulate (Map.empty, [], []) (T.Polymorphic . snd <$> qPairs)
    return $ Forall tvars (T.Qualified $ bs :| cs :=> T.Record (Ann.decorate . T.Polymorphic <$$> qPairs))

  generalize (T.Union tys) = do
    tys' <- forM tys (AST.node |> generalize)
    let (bs, cs, tvars) = foldl accumulate (Map.empty, [], []) (fmap T.Polymorphic tys')
    return $ Forall tvars (T.Qualified $ bs :| cs :=> T.Union (Ann.decorate . T.Polymorphic <$> tys'))

  generalize (T.Arrow arg out) = do
    Forall tvars arg' <- generalize arg
    return $ Forall tvars (arg' `T.Arrow` out)

  generalize (T.Var tvar) = return $ Forall [tvar] (T.Var tvar)

  generalize (T.Qualified (bs :| cs :=> t)) = do
    let (bs, cs, tvars) = accumulate (Map.empty, [], []) t
    return $ Forall tvars (T.Qualified $ bs <> bs :| cs <> cs :=> t)

  generalize ty = case ty of
    T.Singleton lit -> implement $ case lit of
      LString _ -> "IsString"
      LBool _   -> "IsBool"
      LInt _    -> "Num"
    T.Data prim -> implement $ case prim of
      "Int"    -> "Num"
      "String" -> "IsString"
      "Bool"   -> "IsBool"
    _ -> return $ Forall [] ty

    where
      implement :: (Eff.State Int :> es) => ProtocolID -> Eff es (Q.Polymorphic Type)
      implement protocol = do
        tvar <- fresh
        return $ Forall [tvar] (T.Qualified $ Map.empty :| [T.Var tvar `T.Implements` protocol] :=> T.Var tvar)

      fresh :: (Eff.State Int :> es) => Eff es (Variable Type)
      fresh = do
        i <- Eff.get
        Eff.modify @Int (+1)
        let count = show ([1 ..] !! i)
        let tvar = T.Poly ("g" ++ count) K.Type
        return tvar


accumulate (bs, cs, tvars) (T.Polymorphic (Forall tvars' t)) = accumulate (bs, cs, nub $ tvars ++ tvars') t
accumulate (bs, cs, tvars) (T.Qualified (bs' :| cs' :=> t)) = accumulate (bs <> bs', nub $ cs ++ cs',  tvars) t
accumulate (bs, cs, tvars) (T.Tuple (fmap AST.node -> tys)) = foldl accumulate (bs, cs, tvars) tys
accumulate (bs, cs, tvars) (T.Union (fmap AST.node -> tys)) = foldl accumulate (bs, cs, tvars) tys
accumulate (bs, cs, tvars) (T.Record (fmap (snd |> AST.node) -> tys)) = foldl accumulate (bs, cs, tvars) tys
accumulate (bs, cs, tvars) (T.Applied (AST.node -> t1) (AST.node -> t2)) = foldl accumulate (bs, cs, tvars) [t1, t2]
accumulate (bs, cs, tvars) (T.Arrow t1 t2) = foldl accumulate (bs, cs, tvars) [t1, t2]
accumulate result _ = result
