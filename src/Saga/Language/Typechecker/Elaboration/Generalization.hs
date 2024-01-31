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
import           Saga.Language.Syntax.Elaborated.Kinds             (Kind)
import qualified Saga.Language.Typechecker.Elaboration.Annotations as Ann
import           Saga.Language.Typechecker.Variables               (Variable)
import           Saga.Utils.Common                                 (fmap2,
                                                                    forM2)
import           Saga.Utils.Operators                              ((<$$>),
                                                                    (|>))


instance Generalize Type where
  generalize e | pTrace ("Generalize Type:\n" ++ show e) False = undefined
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
    Forall tvars arg' <- generalize $ AST.node arg
    return $ case arg' of
      T.Qualified (bs :| cs :=> qt) -> Forall tvars (T.Qualified $ bs :| cs :=> annotate qt `T.Arrow` out)
      _ -> Forall tvars (annotate arg' `T.Arrow` out)
    where
      annotate t = AST.Annotated t (AST.annotation arg)

  generalize (T.Var tvar) = return $ Forall [tvar] (T.Var tvar)

  generalize (T.Qualified (bs :| cs :=> t)) = do
    let (bs, cs, tvars) = accumulate (Map.empty, [], []) t
    return $ Forall tvars (T.Qualified $ bs <> bs :| cs <> cs :=> t)

  generalize (T.Polymorphic t) = return t

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
        Eff.modify @Int (+1)
        i <- Eff.get
        let count = show ([1 ..] !! i)
        let tvar = T.Poly ("g" ++ count) K.Type
        return tvar


instance Generalize Kind where
  generalize e | pTrace ("Generalize Kind:\n" ++ show e) False = undefined

  generalize (K.Protocol k) = do
    Forall kvars k' <- generalize $ AST.node k
    return $ Forall kvars (K.Protocol $ AST.Raw k')

  generalize (K.Arrow arg out) = do
    Forall tvars arg' <- generalize $ AST.node arg
    return $ Forall tvars (AST.Raw arg' `K.Arrow` out)

  generalize (K.Application cons args) = do
    Forall kvars cons' <- generalize $ AST.node cons
    args' <- forM args (generalize . AST.node)
    let (kvars', fmap AST.Raw -> ks) = unzip $ fmap (\(Forall vars k) -> (vars, k)) args'
    return $ Forall (nub $ kvars ++ concat kvars') (K.Application (AST.Raw cons') ks)

  generalize (K.Var kvar) = return $ Forall [kvar] (K.Var kvar)
  generalize (K.Polymorphic k) = return k

  generalize k = return $ Forall [] k



accumulate (bs, cs, tvars) (T.Polymorphic (Forall tvars' t)) = accumulate (bs, cs, nub $ tvars ++ tvars') t
accumulate (bs, cs, tvars) (T.Qualified (bs' :| cs' :=> t)) = accumulate (bs <> bs', nub $ cs ++ cs',  tvars) t
accumulate (bs, cs, tvars) (T.Tuple (fmap AST.node -> tys)) = foldl accumulate (bs, cs, tvars) tys
accumulate (bs, cs, tvars) (T.Union (fmap AST.node -> tys)) = foldl accumulate (bs, cs, tvars) tys
accumulate (bs, cs, tvars) (T.Record (fmap (snd |> AST.node) -> tys)) = foldl accumulate (bs, cs, tvars) tys
accumulate (bs, cs, tvars) (T.Applied (AST.node -> t1) (AST.node -> t2)) = foldl accumulate (bs, cs, tvars) [t1, t2]
accumulate (bs, cs, tvars) (T.Arrow (AST.node -> t1) (AST.node -> t2)) = foldl accumulate (bs, cs, tvars) [t1, t2]
accumulate result _ = result
