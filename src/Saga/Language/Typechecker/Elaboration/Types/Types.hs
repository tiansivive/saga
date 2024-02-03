{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

module Saga.Language.Typechecker.Elaboration.Types.Types where

import           Control.Monad                                        (foldM,
                                                                       forM)
import qualified Effectful.State.Static.Local                         as Eff
import qualified Saga.Language.Syntax.AST                             as NT (NodeType (..))
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds                as EK
import qualified Saga.Language.Syntax.Elaborated.Types                as EL
import qualified Saga.Language.Syntax.Reduced.AST                     as RD

import qualified Saga.Language.Syntax.Reduced.Types                   as RD
import           Saga.Language.Syntax.Reduced.Types                   (TypeExpr)
import qualified Saga.Language.Typechecker.Elaboration.Effects        as Effs

import           Saga.Language.Typechecker.Elaboration.Monad          (Effects,
                                                                       Elaboration (..),
                                                                       Generalize (..),
                                                                       Instantiate (instantiate))
import qualified Saga.Language.Typechecker.Elaboration.Types.Shared   as Shared
import           Saga.Utils.Common                                    (forM2)

import           Saga.Language.Typechecker.Elaboration.Generalization
import           Saga.Language.Typechecker.Variables                  (Variable)

import           Data.Functor                                         ((<&>))
import qualified Data.List                                            as List
import qualified Data.Map                                             as Map
import           Effectful                                            (Eff)
import qualified Effectful.Error.Static                               as Eff
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.Writer.Static.Local                        as Eff
import           Prelude                                              hiding
                                                                      (id,
                                                                       lookup)
import           Saga.Language.Syntax.AST                             (Phase (..))
import           Saga.Language.Syntax.Polymorphism                    (Given (..),
                                                                       Polymorphic (..),
                                                                       Qualified (..))
import           Saga.Language.Typechecker.Elaboration.Effects        (State (..))
import           Saga.Language.Typechecker.Elaboration.Types.Kinds    hiding
                                                                      (Protocol)
import           Saga.Language.Typechecker.Elaboration.Types.Shared   (Tag (..))
import           Saga.Language.Typechecker.Env                        (CompilerState (..))
import           Saga.Language.Typechecker.Errors                     (Exception (..),
                                                                       SagaError (..),
                                                                       crash)
import           Saga.Language.Typechecker.Protocols                  (Protocol (..))
import qualified Saga.Language.Typechecker.Solving.Constraints        as Solver
import           Saga.Utils.Operators                                 ((|$>),
                                                                       (|>),
                                                                       (||>))
-- TODO: This is kind inference via elaboration of types.

instance Elaboration NT.Type where
  type Effects NT.Type es = Effs.Elaboration es

  elaborate (RD.Raw node) = case node of
    RD.Identifier x -> lookup x

    RD.Singleton lit -> return $ EL.Annotated (EL.Singleton lit) (EL.Raw EK.Type)

    RD.Arrow t1 t2 -> do
      t1' <- elaborate $ RD.Raw t1
      t2' <- elaborate $ RD.Raw t2
      ev <- Shared.fresh' E
      Eff.tell $ Solver.Equality ev (Solver.K $ Shared.extract t1') (Solver.K EK.Type)
      return $ EL.Annotated (EL.Arrow t1' t2') (EL.Raw EK.Type)

    RD.Clause tyExpr bindings -> do
        ty' <- elaborate $ RD.Raw tyExpr

        bindings' <- binds
        constraints' <- forM constraints (elaborate |$> EL.node)

        return $ EL.Annotated (EL.Qualified (bindings' :| constraints' :=> EL.node ty')) (EL.Raw $ Shared.extract ty')

        where
            locals = [ (id, RD.Raw ty) | RD.Bind id ty <- bindings]
            constraints = [ RD.Raw c | RD.Constraint c <- bindings]

            binds = fmap Map.fromList $
              forM locals $ \(id, ty) ->
                elaborate ty >>= \t ->
                  return (EL.Local id $ Shared.extract t, EL.node t)

    RD.Implementation prtcl (RD.Raw -> tyExpr) -> do
        Saga { protocols } <- Eff.ask @(CompilerState Elaborated)
        case List.find (\(Protocol { id }) -> id == prtcl) protocols of
            Nothing                -> error $ "Could not find Protocol: " ++ prtcl
            Just (Protocol { spec }) -> do
              ty <- elaborate tyExpr
              crash $ NotYetImplemented "Elaboration of Protocol Implementation"
              -- TODO:SUGGESTION Add one more constraint type, to evaluate this application and add the implementation to the environment
              -- We also need some way to get the underlying value expression in order to add it to the environment
              -- Will probably be handled when elaborating annotated expressions
              -- RD.Raw $ RD.Application spec

    RD.Lambda params (RD.Raw -> body) ->  do
      tvars' <- tvars
      let pairs = tvars' ||> fmap (\tvar@(EL.Poly p k) -> (p, EL.Annotated (EL.Var tvar) (EL.Raw k)))
      Eff.local (\e -> e { types = Map.fromList pairs `Map.union` types e  }) $ do
        body' <- elaborate body
        return $ EL.Annotated (EL.Polymorphic $ Forall tvars' (EL.node body')) (EL.Raw $ Shared.extract body')

      where
        tvars = sequence $ do
          p <- params
          return $ Shared.fresh' K >>= \kvar -> let k = EK.Var kvar in return $ EL.Poly p k

    FieldAccess (RD.Raw -> record) field -> do
        record' <- elaborate record
        case EL.node record' of
            EL.Record pairs -> maybe (Eff.throwError $ not_field record') return (List.lookup field pairs)
            _             -> Eff.throwError $ Fail $ "Type\n\t" ++ show record' ++ "\nis not a Record when trying to access field\n\t" ++ field
        where
            not_field r = Fail $ field ++ " is not a field of " ++ show r

    RD.Application (RD.Raw -> f) [RD.Raw -> arg] -> do
      sub@(EL.Unification _ k) <- Shared.fresh' T
      out <- EK.Var <$> Shared.fresh' K
      f' <- elaborate f
      arg' <- elaborate arg

      inferred <- generalize' $ EL.annotation arg' `EK.Arrow` EL.Raw out
      evidence <- Shared.fresh' E
      Eff.tell $ Solver.Equality evidence (Solver.K $ Shared.extract f') (Solver.K $ EK.Polymorphic inferred)
      Eff.tell $ Solver.Equality evidence (Solver.K out) (Solver.K k)

      Eff.tell $ Solver.Evaluate (EL.Var sub) (Solver.Application (EL.node f') (EL.node arg'))
      return $ EL.Annotated (EL.Var sub) (EL.Raw out)

      where
        generalize' k = do
          count <- Eff.gets kvars
          (inferred, count') <- Eff.runState count $ generalize k
          Eff.modify $ \s -> s { kvars = count' }
          return inferred

    RD.Application f (a : as) -> elaborate curried
            where
                partial = RD.Application f [a]
                curried = RD.Raw $ foldl (\f' a -> RD.Application f' [a]) partial as


    RD.Match (RD.Raw -> subject) _  -> do
      Saga { types, kinds } <- Eff.ask

      subject' <- elaborate subject
      sub@(EL.Unification _ k) <- Shared.fresh' T
      -- | TODO: Most likely we need a specific structure for the match clause rather than a closure
      Eff.tell $ Solver.Evaluate (EL.Var sub) (Solver.Match node (Solver.Scope types kinds))

      return $ EL.Annotated (EL.Var sub) (EL.Raw k)

    RD.Union tys -> do
      tys' <- forM tys (elaborate . RD.Raw)
      return $ EL.Annotated (EL.Union tys') (EL.Raw EK.Type)
    RD.Tuple elems -> do
      tys' <- forM elems (elaborate . RD.Raw)
      return $ EL.Annotated (EL.Tuple tys') (EL.Raw EK.Type)
    RD.Record pairs -> do
      pairs' <- forM2 pairs (elaborate . RD.Raw)
      return $ EL.Annotated (EL.Record pairs') (EL.Raw EK.Type)
    atom  -> crash $ NotYetImplemented $ "Evaluation of type atom: " ++ show atom


instance Elaboration NT.Constraint where
  type Effects NT.Constraint es = Effs.Elaboration es

  elaborate (RD.Raw constraint) = case constraint of
    RD.Implements (RD.Raw -> ty) prtcl -> do
      t <- elaborate ty
      return $ EL.Annotated (EL.Implements (EL.node t) prtcl) (EL.Raw EK.Constraint)
    RD.Refinement (fmap RD.Raw -> bs) liquid (RD.Raw -> ty) -> do
      t <- elaborate ty
      bs' <- forM bs elaborate
      let refinement = EL.Refinement (EL.node <$> bs') liquid (EL.node t)
      return $ EL.Annotated refinement (EL.Raw EK.Constraint)

  elaborate (RD.Annotated constraint k) = do
    k' <- elaborate k
    c <- elaborate (RD.Raw constraint)
    ev <- Shared.fresh' E
    Eff.tell $ Solver.Equality ev (Solver.K $ Shared.extract c) (Solver.K $ EL.node k')
    return $ EL.Annotated (EL.node c) k'



lookup :: Effs.Elaboration es => String -> Eff es (EL.AST Elaborated NT.Type)
lookup id = do
    Saga { types } <- Eff.ask
    maybe (Eff.throwError $ UndefinedIdentifier id) return $ Map.lookup id types



pattern FieldAccess record field = RD.Application (RD.Identifier ".") [record,  RD.Identifier field]
