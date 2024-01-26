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

import           Data.List                                         (nub)
import           Debug.Pretty.Simple                               (pTrace)
import qualified Saga.Language.Syntax.Elaborated.Kinds             as K
import qualified Saga.Language.Typechecker.Elaboration.Annotations as Ann
import           Saga.Language.Typechecker.Variables               (Variable)


instance Generalize Type where
  generalize e | pTrace ("Generalize:\n" ++ show e) False = undefined
  generalize (T.Tuple tys) = do
    ts <- mapM (generalize . extract) tys
    let (bs, cs, tvars, ts') = foldl accumulate (Map.empty, [], [], []) ts
    return $ Forall tvars (bs :| cs :=> T.Tuple (fmap Ann.decorate ts'))
    where
      accumulate (bs, cs, tvars, ts) (Forall tvars' (bs' :| cs' :=> t)) = (bs <> bs', nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ ts ++ [t])
  generalize (T.Record pairs) = do
    qPairs <- mapM (mapM (generalize . extract)) pairs
    let (bs, cs, tvars, pairs') = foldl accumulate (Map.empty, [], [], []) qPairs
    return $ Forall tvars (bs :| cs :=> T.Record (fmap (fmap Ann.decorate) pairs'))
    where
      accumulate (bs, cs, tvars, pairs) (key, Forall tvars' (bs' :| cs' :=> t)) = (bs <> bs', nub $ cs ++ cs', nub $ tvars ++ tvars', nub $ pairs ++ [(key, t)])

  generalize (T.Arrow arg out) = do
    Forall tvars (cs :=> arg') <- generalize arg
    return $ Forall tvars (cs :=> arg' `T.Arrow` out)

  generalize (T.Union tys) = do
    tys' <- mapM (generalize . extract) tys
    let (tvars, bs, cs, qts) = mapM (\(Forall tvars (bs :| cs :=> qt)) -> (tvars, bs, cs, qt)) tys'
    return $ Forall (nub tvars) (bs :| nub cs :=> T.Union (fmap Ann.decorate (nub qts)))

  generalize (T.Var tvar) = return $ Forall [tvar] (Map.empty :| [] :=> T.Var tvar)
  generalize ty = case ty of
    T.Singleton lit -> implement $ case lit of
      LString _ -> "IsString"
      LBool _   -> "IsBool"
      LInt _    -> "Num"
    T.Data prim _ -> implement $ case prim of
      "Int"    -> "Num"
      "String" -> "IsString"
      "Bool"   -> "IsBool"
    _ -> return $ Forall [] (Q.none :=> ty)

    where
      implement :: (Eff.State Int :> es) => ProtocolID -> Eff es (Q.Polymorphic Type)
      implement protocol = do
        tvar <- fresh
        return $ Forall [tvar] (Map.empty :| [T.Var tvar `T.Implements` protocol] :=> T.Var tvar)

      fresh :: (Eff.State Int :> es) => Eff es (Variable Type)
      fresh = do
        i <- Eff.get
        Eff.modify @Int (+1)
        let count = show ([1 ..] !! i)
        let tvar = T.Poly ("g" ++ count) K.Type
        return tvar




extract (AST.Raw t)         = t
extract (AST.Annotated t _) = t

