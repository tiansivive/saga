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
import qualified Effectful.Error.Static                               as Eff
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.Writer.Static.Local                        as Eff
import           Prelude                                              hiding
                                                                      (id)
import           Saga.Language.Syntax.AST                             (Phase (..))
import           Saga.Language.Syntax.Polymorphism                    (Given (..),
                                                                       Polymorphic (..),
                                                                       Qualified (..))
import           Saga.Language.Typechecker.Elaboration.Effects        (State (..))
import           Saga.Language.Typechecker.Elaboration.Types.Kinds    hiding
                                                                      (Protocol)
import           Saga.Language.Typechecker.Elaboration.Types.Shared   (mkEvidence)
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
    RD.Identifier x -> _lookup x

    RD.Singleton lit -> return $ EL.Annotated (EL.Singleton lit) (EL.Raw EK.Type)

    RD.Arrow t1 t2 -> do
      t1' <- elaborate $ RD.Raw t1
      t2' <- elaborate $ RD.Raw t2
      ev <- Shared.mkEvidence
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


    RD.Implementation prtcl tyExpr -> do
        Saga { protocols } <- Eff.ask @(CompilerState Elaborated)
        case List.find (\(Protocol { id }) -> id == prtcl) protocols of
            Nothing                -> error $ "Could not find Protocol: " ++ prtcl
            Just (Protocol { spec }) -> elaborate . RD.Raw $ RD.Application spec [tyExpr]

    RD.Lambda params (RD.Raw -> body) ->  do
      tvars' <- tvars
      let pairs = tvars' ||> fmap (\tvar@(EL.Poly p k) -> (p, EL.Annotated (EL.Var tvar) (EL.Raw k)))
      Eff.local (\e -> e { types = Map.fromList pairs `Map.union` types e  }) $ do
        body' <- elaborate body
        return $ EL.Annotated (EL.Polymorphic $ Forall tvars' (EL.node body')) (EL.Raw $ Shared.extract body')

      where
        tvars = sequence $ do
          p <- params
          return $ Shared.fresh >>= \kvar -> let k = EK.Var kvar in return $ EL.Poly p k


    FieldAccess (RD.Raw -> record) field -> do
        record' <- elaborate record
        case EL.node record' of
            EL.Record pairs -> maybe (Eff.throwError $ not_field record') return (List.lookup field pairs)
            _             -> Eff.throwError $ Fail $ "Type\n\t" ++ show record' ++ "\nis not a Record when trying to access field\n\t" ++ field
        where
            not_field r = Fail $ field ++ " is not a field of " ++ show r

    --RD.Application f a -> crash $ NotYetImplemented "Elaboration and evaluation of type applications type expressions"
    RD.Application (RD.Raw -> f) [RD.Raw -> arg] -> do
      out <- EK.Var <$> Shared.fresh
      f' <- elaborate f
      arg' <- elaborate arg

      inferred <- generalize' $ EL.annotation arg' `EK.Arrow` EL.Raw out
      evidence <- Shared.mkEvidence
      Eff.tell $ Solver.Equality evidence (Solver.K $ Shared.extract f') (Solver.K $ EK.Polymorphic inferred)


      return $ case (EL.node f', EL.node arg') of
          (EL.Var tvar, _)                      -> EL.Annotated (EL.Applied f' arg') (EL.Raw out)
          (ty@(EL.Polymorphic poly), EL.Var {}) -> EL.Annotated (EL.Applied f' arg') (EL.Raw out)
          (ty@(EL.Polymorphic poly), t)         -> EL.Annotated (instantiate poly t) (EL.Raw out)

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
      --return $ EL.Annotated (EL.Application (EL.node f') (EL.node a')) (EL.Raw EK.Type)
  -- do
  --       fn' <- evaluate fnExpr
  --       args <- mapM evaluate argExprs

  --       case fn' of
  --           -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here

  --           t@(T.Data {}) -> return . node $ application t args
  --           -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here
  --           t@(T.Applied {}) -> return . node $ application t args
  --           T.Closure closure env -> _apply closure args
  --           T.Var (T.Poly  t _) -> do
  --               T.Raw t' <- lookup t
  --               case t' of
  --                   -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here
  --                   T.Data tycon -> return . node $ application t' args
  --                   -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here
  --                   T.Applied cons arg -> return . node $ application t' args
  --                   T.Closure closure env -> _apply closure args
  --                   _ -> Eff.throwError $ UnexpectedType t' "Cannot apply this variable expression"
  --           _ -> Eff.throwError $ UnexpectedType fn' "Cannot apply this type expression"

  --       where
  --           application (T.Raw -> t) (fmap T.Raw -> args) = foldl (T.Applied |>: T.Raw ) t args
  --            apply :: TypeExpr -> [Variable Type] -> [Type] -> EvaluationM Type
            -- apply tyExpr [] [] = evaluate tyExpr
            -- apply tyExpr tvars [] = do
            --     Saga { types, kinds } <- Eff.ask
            --     return $ T.Closure tyExpr (T.Scope types kinds)

            -- apply tyExpr (p@(T.Poly id k) :params) (a :args) = Eff.local (\e -> e{ types = Map.insert id (T.Raw $ T.Var p) (types e) }) $ do
            --     apply tyExpr params args
            -- apply _ [] (a: args) = Eff.throwError $ TooManyArguments fnExpr argExprs

    RD.Match t cases  -> crash $ NotYetImplemented "Elaboration and evaluation of Match type expressions"

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
    ev <- mkEvidence
    Eff.tell $ Solver.Equality ev (Solver.K $ Shared.extract c) (Solver.K $ EL.node k')
    return $ EL.Annotated (EL.node c) k'

--   elaborate (AST.Raw node) = case node of
--     RD.Data name -> do
--       k <- Shared.lookup name
--       return $ EL.Annotated (ET.Data name) k

--     RD.Singleton lit -> return $ EL.Annotated (ET.Singleton lit) (EL.Raw EK.Type)
--     RD.Tuple elems -> do
--       elems' <- forM elems elaborate
--       return $ EL.Annotated (ET.Tuple elems') (EL.Raw EK.Type)
--     RD.Record pairs ->  do
--       pairs' <- forM2 pairs elaborate
--       return $ EL.Annotated (ET.Record pairs') (EL.Raw EK.Type)
--     RD.Union elems -> do
--       elems' <- forM elems elaborate
--       return $ EL.Annotated (ET.Union elems') (EL.Raw EK.Type)
--     RD.Arrow in' out -> do
--       in'' <- elaborate $ AST.Raw in'
--       out' <- elaborate $ AST.Raw out
--       return $ EL.Annotated (ET.Arrow in'' out') (EL.Raw EK.Type)

--     RD.Applied cons arg -> do
--       out <- EK.Var <$> Shared.fresh
--       cons' <- elaborate cons
--       arg' <- elaborate arg

--       inferred <- generalize' $ EL.annotation arg' `EK.Arrow` EL.Raw out
--       return $ EL.Annotated (ET.Applied cons' arg') (EL.Raw EK.Type)
--       where
--         generalize' ty = do
--           count <- Eff.gets kvars
--           (inferred, count') <- Eff.runState count $ generalize ty
--           Eff.modify $ \s -> s { kvars = count' }
--           return inferred

--     RD.Var kvar -> do
--       (kvar', ann) <- elaborateVar kvar
--       return $ EL.Annotated (ET.Var kvar') (EL.Raw ann)

--     RD.Polymorphic (Forall tvars (AST.Raw -> t)) -> do
--       tvars' <- forM tvars elaborateVar
--       t' <- elaborate t
--       let poly = ET.Polymorphic $ Forall (fmap fst tvars') (EL.node t')
--       return $ EL.Annotated poly (EL.annotation t')
--       where
--         fresh' map tvar = Shared.fresh <&> \kvar -> map ||> Map.insert tvar (EL.Raw $ EK.Var kvar)
--     RD.Qualified ty -> crash $ NotYetImplemented "Elaborating Qualified types is not supported yet."

--   elaborate t = undefined





-- elaborateVar v        = case v of
--   RD.Poly name node        -> elaborate' ET.Poly name node
--   RD.Existential name node -> elaborate' ET.Existential name node
--   RD.Local name node       -> elaborate' ET.Local name node
--   where
--     elaborate' cons name (AST.Raw -> k) = do
--       k' <- elaborate k
--       return (ET.Poly name (EL.node k'), EL.node k')



pattern FieldAccess record field = RD.Application (RD.Identifier ".") [record,  RD.Identifier field]
