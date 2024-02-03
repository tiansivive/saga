module Saga.Language.Typechecker.Solving.Protocols where
import qualified Data.Map                                            as Map
import           Data.Maybe                                          (isJust)
import           Effectful                                           (Eff)
import qualified Effectful.Error.Static                              as Eff
import qualified Effectful.State.Static.Local                        as Eff
import qualified GHC.List                                            as List
import qualified Saga.Language.Syntax.Elaborated.Types               as T
import           Saga.Language.Syntax.Polymorphism                   (Given (..),
                                                                      Polymorphic (..),
                                                                      Qualified (..))
import           Saga.Language.Typechecker.Errors                    (Exception (..),
                                                                      SagaError (..),
                                                                      crash)
import qualified Saga.Language.Typechecker.Solving.Constraints       as Solver
import           Saga.Language.Typechecker.Solving.Constraints       (Evidence (..))
import           Saga.Language.Typechecker.Solving.Monad             (Solution (..),
                                                                      Solving,
                                                                      Status (..))
import           Saga.Utils.Operators                                ((|>),
                                                                      (||>))


import qualified Saga.Language.Typechecker.Protocols                 as P

import           Control.Monad                                       ((>=>))
import qualified Data.List                                           as List
import qualified Effectful.Reader.Static                             as Eff
import           Prelude                                             hiding (id)
import           Saga.Language.Syntax.AST                            (Phase (..))
import           Saga.Language.Typechecker.Elaboration.Monad         (Instantiate (..))
import           Saga.Language.Typechecker.Env                       (CompilerState (..))
import           Saga.Language.Typechecker.Protocols                 (Protocol (..))
import qualified Saga.Language.Typechecker.Solving.Shared            as Shared
import           Saga.Language.Typechecker.Solving.Shared            (Tag (..))
import           Saga.Language.Typechecker.Substitution              (Substitutable (..),
                                                                      mkSubst)

import           Saga.Language.Typechecker.Elaboration.Instantiation


solve :: Solving es => Solver.Constraint -> Eff es (Status, Solver.Constraint)
solve (Solver.Implementation (Solver.Evidence ev) t@(T.Polymorphic poly) prtcl) = crash . NotYetImplemented $ "Solving Implementation Constraint for Polymorphic type:\n" ++ show t

solve (Solver.Implementation e@(Solver.Evidence ev) ty prtcl) =
    case ty of
        T.Var (T.Poly {})           -> return (Deferred, Solver.Implementation e ty prtcl)
        T.Var (T.Existential id k)  -> crash $ NotYetImplemented "Solving Implementation constraint for existential type Vars"
        T.Var (T.Local id k)        -> crash $ NotYetImplemented "Solving Implementation for locally scoped type Vars"

        t -> do
            impl <- Eff.asks @(CompilerState Elaborated) $ protocols
                |> List.find (\P.Protocol { id } -> id == prtcl) >=>
                    -- | HACK This needs to search via unification. How to make the monads fit though?
                    implementations |> List.find (\(P.Implementation (id, t', expr)) -> t == t')


            impl ||> maybe
                (Eff.throwError $ MissingProtocolImplementation prtcl t)
                (\impl'@(P.Implementation (id, ty', expr)) -> do
                    Shared.update E $ Map.singleton e (Solver.Protocol impl')
                    case ty' of
                        T.Qualified (_ :| cs :=> _) -> (Solved,) <$> Shared.propagate cs
                        _ -> return (Solved, Solver.Empty)
                )



-- | FIXME #22 @tiansivive Entailment: Check if current protocol is implied by any other implementation constraint. If so, it's safe to remove
entails :: Solving es => Solver.Constraint -> [Solver.Constraint] -> Eff es [Solver.Constraint]
entails i@(Solver.Implementation e t pid) cs = do
    env@Solution { evidence, witnessed } <- Eff.get

    evar <- case exists evidence of
        []       -> return e
        [evar]   -> return evar
        multiple -> Eff.throwError $ MultipleImplementationEvidence t pid

    Eff.modify @Solution $ \s -> s { witnessed = witnessed `Map.union` next witnessed evar }
    return $ fmap (normalize evars evar) cs

    where
        next ws e = evars ||>List.filter (\e -> e `notElem` Map.keys ws ) |> fmap (,e) |> Map.fromList
        exists evidence = evars ||> List.filter (\var -> isJust $ Map.lookup var evidence)

        evars = fmap snd impls
        impls = [ (c, e') | c@(Solver.Implementation e' t' pid') <- cs
                    , t == t'
                    , pid == pid'
                    , e /= e'

                ]

        normalize es e (Solver.Implementation e' t' pid') | e' `elem` es   = Solver.Implementation e t' pid'
        normalize _ _ (Solver.Implementation e t pid)                      = Solver.Implementation e t pid
        normalize _ _ constraint                             = constraint



simplify' :: Solving es => Solver.Constraint -> Eff es Solver.Constraint
simplify' impl@(Solver.Implementation ev t prtcl) = do
    Solution { evidence, tvars } <- Eff.get
    process evidence tvars

    where
        process evidence tvars
            | isJust $ Map.lookup ev evidence   = return Solver.Empty
            | T.Var tvar <- t
            , isJust $ Map.lookup tvar tvars    = return $ Solver.Implementation ev (apply tvars t) prtcl
            | otherwise                         = flatten impl


flatten :: Solving es => Solver.Constraint -> Eff es Solver.Constraint
flatten c@(Solver.Implementation ev ty prtcl) = do
    env <- Eff.ask @(CompilerState Elaborated)
    P.Protocol { spec } <- env ||> protocols
        |> List.find (\P.Protocol { id } -> id == prtcl)
        |> maybe (Eff.throwError $ UndefinedIdentifier prtcl) return


    Solver.Conjunction c <$> case instantiate spec ty of
        T.Qualified (_ :| cs :=> _) -> Shared.propagate cs
        _                           -> return Solver.Empty





          --  _ -> crash $ NotYetImplemented $ "Flattening Solver.Constraint for " ++ show item
