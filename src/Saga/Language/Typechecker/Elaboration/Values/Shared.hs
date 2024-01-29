{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Saga.Language.Typechecker.Elaboration.Values.Shared where



import           Effectful                                            (Eff,
                                                                       (:>))
import qualified Effectful.Error.Static                               as Eff
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.State.Static.Local                         as Eff
import qualified Effectful.Writer.Static.Local                        as Eff

import qualified Saga.Language.Syntax.AST                             as NT (NodeType (..))
import           Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL

import qualified Saga.Language.Syntax.Elaborated.Kinds                as K
import qualified Saga.Language.Syntax.Elaborated.Types                as T
import           Saga.Language.Syntax.Elaborated.Types                (Type (..))
import           Saga.Language.Syntax.Polymorphism                    (Given (..),
                                                                       Polymorphic (..),
                                                                       Qualified (..))

import           Saga.Language.Typechecker.Elaboration.Monad          (Instantiate (..))
import           Saga.Language.Typechecker.Elaboration.Values.Effects (State (..))

import           Saga.Language.Typechecker.Env                        (CompilerState (..),
                                                                       Proofs (..))
import           Saga.Language.Typechecker.Errors                     (SagaError (UnboundVariable))

import qualified Saga.Language.Typechecker.Solving.Constraints        as CST
import           Saga.Language.Typechecker.Solving.Constraints        (Evidence)

import qualified Saga.Language.Typechecker.Variables                  as Var
import           Saga.Language.Typechecker.Variables                  (Variable)

import           Saga.Utils.Operators                                 ((|>),
                                                                       (||>))
import           Saga.Utils.TypeLevel                                 (type (§))

import qualified Data.Map                                             as Map

import           Control.Monad                                        (forM)
import           Data.Functor                                         ((<&>))
import           Data.Map                                             (Map)
import qualified Data.Set                                             as Set
import           Saga.Language.Typechecker.Elaboration.Instantiation
import           Saga.Language.Typechecker.Solver.Substitution        (Subst,
                                                                       Substitutable (..),
                                                                       compose,
                                                                       mkSubst,
                                                                       nullSubst)

import qualified Saga.Language.Typechecker.Elaboration.Traversals     as Traverse
import           Saga.Language.Typechecker.Elaboration.Traversals


lookup ::
  (Eff.Reader (CompilerState 'Elaborated) :> es, Eff.Reader Var.Level :> es, Eff.State State :> es, Eff.Error SagaError :> es)
  => String -> Eff es (AST 'Elaborated 'Type)
lookup x = do
  Saga { types, extra } <- Eff.ask

  ty <- case Map.lookup x types of
    Just (EL.Raw node)           -> decorate <$> walk node
    Just (EL.Annotated node ann) -> EL.Annotated <$> walk node <*> pure ann
    Nothing                      -> Eff.throwError $ UnboundVariable x

  case Map.lookup (EL.node ty) (narrowings extra) of
    Just t' -> return $ EL.Annotated t' (EL.annotation ty)
    Nothing -> return ty

  where
    walk (T.Polymorphic poly) = fresh T.Unification >>= T.Var |> instantiate poly |> walk
    walk t                    = return t


contextualize :: (Eff.State State :> es, Eff.Reader Var.Level :> es) => Type -> Eff es CST.Constraint
contextualize ty@(T.Qualified (bs :| cs :=> t)) = do
  (cs', sub) <- Eff.runState @(Subst Type) nullSubst . Eff.runReader bs $ concat <$> forM (locals t) collect

  assumptions <- forM cs toCST
  implied <- forM cs' toCST

  eqs <- forM (Map.toList sub) (\(local, t) -> do
    lvl <- Eff.asks @Var.Level (+1)
    Eff.modify $ \s -> s { levels = Map.insert local lvl $ levels s }
    ev <- mkEvidence
    return $ CST.Equality ev (CST.Var local) (toItem t))

  return $ CST.Implication (locals t) (fmap CST.Assume assumptions) (scoped $ implied <> eqs)

  where
    scoped = foldl CST.Conjunction CST.Empty

    toCST (ty `T.Implements` protocol) = mkEvidence <&> \e -> CST.Impl e (toItem ty) protocol
    toCST (T.Refinement binds liquid ty) = return $ CST.Refined (toItem <$> binds) (toItem ty) liquid

locals :: Type -> [Variable Type]
locals t = [ v | v@(T.Local {}) <- Set.toList $ ftv t ]


collect :: (Eff.Reader Bindings :> es, Eff.State (Subst Type) :> es) => Variable Type -> Eff es [Node 'Elaborated 'Constraint]
collect v@(T.Local {}) = do
  bs <- Eff.ask
  case Map.lookup v bs of
    Just (T.Qualified (bs' :| cs :=> t))
      | null $ locals t -> do
        Eff.modify $ \sub -> mkSubst (v, t) `compose` sub
        return cs
      | otherwise -> do
        cs' <- concat <$> forM (locals t) collect
        return $ cs <> cs'
    Just t -> return []




fresh :: (Eff.Reader Var.Level :> es, Eff.State State :> es) => Constructor -> Eff es § Variable Type
fresh constructor = do
  iType <- Eff.gets $ tvars |> (+1)
  Eff.modify $ \s -> s { tvars = iType }
  let typeCount = show ([1 ..] !! iType)

  iKind <- Eff.gets $ kvars |> (+1)
  Eff.modify $ \s -> s { kvars = iKind }
  let kindCount = show ([1 ..] !! iKind)
  let kvar = K.Var $ K.Unification ("k" ++ kindCount)
  let tvar = constructor ("t" ++ typeCount) kvar
  lvl <- Eff.ask @Var.Level
  Eff.modify $ \s -> s { levels = Map.insert tvar lvl $ levels s }
  return tvar




mkEvidence :: (Eff.State State :> es) => Eff es § Variable CST.Evidence
mkEvidence = do
  i <- Eff.gets $ evars |> (+1)
  Eff.modify $ \s -> s { evars = i }
  let count = show ([1 ..] !! i)
  return $ CST.Evidence ("ev_" ++ count)


toItem :: Type -> CST.Item
toItem (T.Var tvar) = CST.Var tvar
toItem t            = CST.Mono t


extract :: Show (Node 'Elaborated e) => AST 'Elaborated e -> Node 'Elaborated (Annotation e)
extract = EL.annotation |> EL.node

-- QUESTION: Do singletons have a different kind?
decorate :: Node 'Elaborated 'Type -> AST 'Elaborated 'Type
decorate ty@(T.Singleton {})              = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Tuple {})                  = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Record {})                 = EL.Annotated ty $ EL.Raw K.Type

decorate ty@(T.Var (T.Unification t k))   = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Instantiation t k)) = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Poly t k))          = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Local t k))         = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Rigid t k))         = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Scoped t k))        = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Skolem t k))        = EL.Annotated ty $ EL.Raw k

--pattern Raw

type Bindings = Map (Variable Type) Type
type Constructor = String -> K.Kind -> Variable Type

