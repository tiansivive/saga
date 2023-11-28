{-# LANGUAGE DataKinds #-}
module Saga.Language.Typechecker.Solver.Protocols where
import           Control.Monad.Except

import qualified Data.Map                                               as Map
import           Prelude                                                hiding
                                                                        (id)
import           Saga.Language.Typechecker.Environment                  (CompilerState (..))
import           Saga.Language.Typechecker.Errors                       (Exception (NotYetImplemented, Unexpected),
                                                                         SagaError (..),
                                                                         crash)
import           Saga.Language.Typechecker.Inference.Inference          (Instantiate (instantiate))
import qualified Saga.Language.Typechecker.Protocols                    as P
import           Saga.Language.Typechecker.Protocols                    (Protocol (..),
                                                                         ProtocolID)
import           Saga.Language.Typechecker.Qualification                (Given (..),
                                                                         Qualified (..))
import qualified Saga.Language.Typechecker.Solver.Constraints           as C
import           Saga.Language.Typechecker.Solver.Constraints           (Constraint,
                                                                         Evidence,
                                                                         Item)
import           Saga.Language.Typechecker.Solver.Substitution          (Subst,
                                                                         Substitutable (apply),
                                                                         mkSubst)

import           Control.Monad.RWS                                      (MonadReader (ask),
                                                                         MonadState (get),
                                                                         MonadWriter (tell),
                                                                         asks)
import qualified Data.List                                              as List

import           Data.Functor                                           ((<&>))
import qualified Saga.Language.Typechecker.Evaluation                   as Eval
import qualified Saga.Language.Typechecker.Qualification                as Q
import           Saga.Language.Typechecker.Solver.Monad                 (Solution (..),
                                                                         Solve (..),
                                                                         SolverM,
                                                                         Status (..),
                                                                         Tag (..),
                                                                         update)
import qualified Saga.Language.Typechecker.Solver.Shared                as Shared
import           Saga.Language.Typechecker.Solver.Unification           (Unification (unify))
import qualified Saga.Language.Typechecker.Type                         as T
import           Saga.Language.Typechecker.Type                         (Scheme (..),
                                                                         Type)
import qualified Saga.Language.Typechecker.Variables                    as Var
import           Saga.Language.Typechecker.Variables                    (PolymorphicVar)



import           Data.Maybe                                             (isJust)
import qualified Effectful                                              as Eff
import qualified Effectful.Error.Static                                 as Eff
import qualified Effectful.Reader.Static                                as Eff
import qualified Effectful.State.Static.Local                           as Eff
import           Saga.Utils.Operators                                   ((|>),
                                                                         (||>))

import           Saga.Language.Typechecker.Inference.Type.Instantiation
import           Saga.Language.Typechecker.Solver.Entailment            (Entails (..))

data ImplConstraint = Impl (PolymorphicVar Evidence) Item ProtocolID

instance Solve ImplConstraint where
    solve = solve'
    simplify = simplify'

instance Entails ImplConstraint where
    -- | FIXME Entailment: Check if current protocol is implied by any other implementation constraint. If so, it's safe to remove

    entails i@(Impl e it pid) cs = do
        env@Solution { evidence, witnessed } <- Eff.get

        evar <- case exists evidence of
            []       -> return e
            [evar]   -> return evar
            multiple -> Eff.throwError $ MultipleImplementationEvidence it pid

        Eff.modify @Solution $ \s -> s { witnessed = witnessed `Map.union` next witnessed evar }
        return $ fmap (normalize evars evar) cs

        where
            next ws e = evars ||>List.filter (\e -> e `notElem` Map.keys ws ) |> fmap (,e) |> Map.fromList
            exists evidence = evars ||> List.filter (\var -> isJust $ Map.lookup var evidence)

            evars = fmap snd impls
            impls = [ (c, e') | c@(C.Impl e' it' pid') <- cs
                        , it == it'
                        , pid == pid'
                        , e /= e'

                    ]

            normalize es e (C.Impl e' it' pid') | e' `elem` es   = C.Impl e it' pid'
            normalize _ _ (C.Impl e it pid)                      = C.Impl e it pid
            normalize _ _ constraint                             = constraint



solve' :: ImplConstraint -> SolverM (Status, Constraint)
solve' (Impl e@(Var.Evidence ev) t@(C.Mono ty) prtcl) =
    case ty of
        T.Var (Var.Unification {}) -> return (Deferred, C.Impl e t prtcl)
        T.Var (Var.Local id k)     -> crash $ NotYetImplemented "Solving ImplConstraint for locally scoped type Vars"
        T.Var v                    -> crash $ Unexpected v "Unification Var"

        ty                         -> do
            impl <- Eff.asks $ protocols
                |> List.find (\Protocol { id } -> id == prtcl)
                    -- | TODO This needs to search via unification. How to make the monads fit though?
                    >=> implementations |> List.find (\(P.Implementation (id, Forall _ (_ :=> ty'), expr)) -> ty == ty')


            impl ||> maybe
                (Eff.throwError $ MissingProtocolImplementation prtcl ty)
                (\impl'@(P.Implementation (id, Forall _ (bs :| cs :=> ty'), expr)) -> do
                    update E $ mkSubst (e, C.Protocol impl')
                    result <- propagate cs
                    return (Solved, result)
                )

solve' (Impl e item prtcl)                           = crash $ NotYetImplemented $ "Solving ImplConstraint for " ++ show item


simplify' :: ImplConstraint -> SolverM Constraint
simplify' impl@(Impl ev t prtcl) = do
    Solution { evidence, tvars } <- Eff.get
    process evidence tvars

    where
        process evidence tvars
            | isJust $ Map.lookup ev evidence   = return C.Empty
            | C.Unification uvar <- t
            , isJust $ Map.lookup uvar tvars    = return $ C.Impl ev (C.Mono $ apply tvars (T.Var uvar)) prtcl
            | otherwise                         = flatten impl


flatten :: ImplConstraint -> SolverM Constraint
flatten (Impl ev item prtcl) = do
    env <- Eff.ask
    Protocol { spec } <- env ||> protocols
        |> List.find (\Protocol { id } -> id == prtcl)
        |> maybe (Eff.throwError $ UndefinedIdentifier prtcl) return

    spec' <- Eff.inject $ Eval.evaluate spec


    let t@(T.Forall tvars (bs :| cs :=> qt)) = instantiate spec' ty
    C.Conjunction (impl t) <$> propagate cs

    where
        impl (T.Forall [] (_ :=> qt)) = C.Impl ev (C.Mono qt) prtcl
        impl t@(T.Forall tvars _)     = C.Impl ev (C.Poly t) prtcl
        ty = case item of
            C.Unification u -> T.Var u
            C.Mono ty -> ty

            _ -> crash $ NotYetImplemented $ "Flattening ImplConstraint for " ++ show item


propagate :: [Q.Constraint Type] -> SolverM C.Constraint
propagate = foldM (\acc c -> C.Conjunction acc <$> Shared.from c) C.Empty



