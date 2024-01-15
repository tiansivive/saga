{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}


module Saga.Language.Typechecker.Elaboration.Values.Expressions where


import           Saga.Language.Typechecker.Elaboration.Monad          (Effects,
                                                                       Elaboration (..))

import           Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL

import qualified Saga.Language.Syntax.Elaborated.Types                as ET
import qualified Saga.Language.Syntax.Elaborated.Values               as EL

import qualified Saga.Language.Syntax.Evaluated.AST                   as EV
import qualified Saga.Language.Syntax.Evaluated.Values                as EV

import           Control.Monad                                        (forM,
                                                                       forM_)
import           Control.Monad.Except                                 (ExceptT)

import           Data.Foldable                                        (for_)
import qualified Data.Map                                             as Map
import qualified Effectful.Error.Static                               as Eff
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.State.Static.Local                         as Eff
import qualified Effectful.Writer.Static.Local                        as Eff
import qualified Saga.Language.Syntax.Polymorphism                    as Q
import           Saga.Language.Syntax.Polymorphism                    (Polymorphic (..),
                                                                       Qualified (..))
import qualified Saga.Language.Typechecker.Elaboration.Values.Effects as Effs
import           Saga.Language.Typechecker.Elaboration.Values.Effects (State (..))
import qualified Saga.Language.Typechecker.Elaboration.Values.Shared  as Shared
import           Saga.Language.Typechecker.Environment                (CompilerState (..))
import           Saga.Language.Typechecker.Errors                     (SagaError (..))
import           Saga.Language.Typechecker.Lib                        (listConstructor)
import qualified Saga.Language.Typechecker.Solver.Constraints         as CST
import qualified Saga.Language.Typechecker.Variables                  as Var
import           Saga.Utils.Operators                                 ((||>))


instance Elaboration Expression where
    type Effects Expression es = Effs.Elaboration es
    elaborate (EV.Raw node) = case node of
        EV.Literal lit -> return $ EL.Annotated (EL.Literal lit) (ET.Singleton lit)

        EV.Tuple elems ->  do
            elems' <- forM elems elaborate
            return $ EL.Annotated (EL.Tuple elems') (ET.Tuple $ fmap Shared.extract elems')
        EV.Record pairs ->  do
            pairs' <- forM pairs $ mapM elaborate
            return $ EL.Annotated (EL.Record pairs') (ET.Record $ fmap Shared.extract <$> pairs')

        EV.List elems -> do
            tvar <- Shared.fresh ET.Unification
            elems' <-forM elems elaborate
            let tys = fmap Shared.extract elems'

            forM_ tys $ \t -> do
                ev <- Shared.mkEvidence
                Eff.tell $ CST.Equality ev (CST.Var _tvar) (CST.Mono _t)

            return $ EL.Annotated (EL.List elems') (ET.Applied _listConstructor $ ET.Var tvar)



        EV.Lambda ps@(param : rest) body -> do

            tvar <- ET.Var <$> Shared.fresh ET.Unification
            -- QUESTION: Should we mark type variables in the environment instead of of faking the quantification?
            let qt = Forall [] (Q.none :=> tvar)
            let scoped = Eff.local (\e -> e { types = Map.insert param _qt $ types e })
            scoped $ do
                o@(EL.Annotated _ out') <- elaborate out
                let ty = tvar `ET.Arrow` out'
                let expr = EL.Lambda ps o
                return $ EL.Annotated expr ty

            where
                out = case rest of
                    [] -> body
                    _  -> EV.Raw $ EV.Lambda rest body

        FieldAccess record field -> do
            fieldType <- ET.Var <$> Shared.fresh ET.Unification
            evidence <- Shared.mkEvidence
            record'@(EL.Annotated _ ty) <- elaborate record
            -- | TODO: Add row polymorphism: use a new, different constraint type
            Eff.tell $ CST.Equality evidence (CST.Mono _ty) (CST.Mono $ _dummy ET.Record [(field, fieldType)])
            return $ EL.Annotated (EL.FieldAccess record' field) fieldType

        EV.Application (EV.Raw (EV.Var ".")) args -> Eff.throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args
        app'@(EV.Application fn [arg]) -> do
            out <- ET.Var <$> Shared.fresh ET.Unification
            fn'@(EL.Annotated _ fnTy)   <- elaborate fn
            arg'@(EL.Annotated _ argTy) <- elaborate arg

            -- | TODO: How can we notify the constraint solver that there's a new unification variable
            -- | which stands for the result of this function application
            -- | EDIT: Do we even need to do that?
            inferred <- generalize' $ argTy `ET.Arrow` out
            evidence <- Shared.mkEvidence
            Eff.tell $ CST.Equality evidence (Shared.toItem fnTy) (CST.Poly inferred)

            return $ EL.Annotated (EL.Application fn' [arg']) out
            where
                generalize' e = do
                    count <- Eff.gets tvars
                    (inferred, count') <- Eff.runState count $ _generalize e
                    Eff.modify $ \s -> s { tvars = count' }
                    return inferred

        EV.Application fn (a : as) -> elaborate curried
            where
                partial = EV.Raw $ EV.Application fn [a]
                curried = foldl (\f a -> EV.Raw $ EV.Application f [a]) partial as

        EV.Match scrutinee cases -> do
            scrutinee'@(EL.Annotated _ ty) <- elaborate scrutinee
            cases' <- forM cases (elaborateCase ty)
            let (tvars, tys) = foldl separate ([], []) cases'
            let ty' = case length tys of
                    0 -> Nothing
                    _ -> Just $ ET.Union tys

            for_ ty' $ \t -> do
                ev <- Shared.mkEvidence
                Eff.tell $ CST.Equality ev (Shared.toItem ty) (Shared.toItem t)

            forM_ tvars $ \v -> do
                ev <- Shared.mkEvidence
                Eff.tell $ CST.Equality ev (Shared.toItem v) (maybe (Shared.toItem ty) Shared.toItem ty')

            let out = ET.Union $ fmap Shared.extract cases'
            return $ EL.Annotated (EL.Match scrutinee' cases') out

            where
                elaborateCase scrutineeType (EV.Raw (EV.Case pat expr))  = Eff.local @Var.Level (+1) $ do
                    (pat', tyvars) <- Eff.runWriter $ elaborate pat
                    let (pairs, tvars) = unzip $ tyvars ||> fmap (\(id, tvar) -> ((id, Q.Forall [tvar] (Q.none :=> ET.Var tvar)), tvar))
                    narrowed <- ET.Var <$> Shared.fresh ET.Unification
                    -- TODO:ENHANCEMENT Change to Reader effect
                    Eff.modify $ \s -> s { proofs = Map.insert scrutineeType narrowed $ proofs s }
                    let scoped = Eff.local $ \env -> env { types = Map.fromList _pairs <> types env }
                    scoped $ do

                        (expr'@(EL.Annotated _ ty), constraint) <- Eff.listen @CST.Constraint $ elaborate expr
                        ev <- Shared.mkEvidence
                        -- TODO: Skolemize the scrutinee type and the inferred pattern tvars
                        -- QUESTION: Perhaps we should add another type of constraint here, to specifically prove a refinement rather than relying on equality

                        Eff.tell $ CST.Implication _tvars [assume ev pat' narrowed] constraint
                        Eff.modify $ \s -> s { proofs = Map.delete scrutineeType $ proofs s }
                        return $ EL.Annotated (EL.Case pat' expr') ty

                        where
                            assume ev ty ty' = CST.Assume $ CST.Equality ev (Shared.toItem ty) (Shared.toItem ty')


                separate (tvars, tys) caseExpr = case caseExpr of
                    EL.Annotated _ ty@(ET.Var _) -> (ty:tvars, tys)
                    EL.Annotated _ ty            -> (tvars, ty:tys)

        EV.Block stmts -> walk [] stmts
            where
                walk processed [] = return $ EL.Annotated (EL.Block $ reverse processed) returnTy
                    where returnTy = case head processed of
                            EL.Annotated (EL.Return {}) ty -> ty
                            _                              -> ET.Void
                walk processed ((EV.Raw stmt) : rest) = case stmt of

                    EV.Return expr                 -> do
                        annotated@(EL.Annotated _ ty) <- elaborate expr
                        walk (EL.Annotated (EL.Return annotated) ty : processed) []

                    EV.Declaration (EV.Type id ty) -> do
                        ty' <- elaborate ty
                        Eff.local (\e -> e{ types = Map.insert id _ty' $ types e }) $ do
                            walk (EL.Raw (EL.Declaration $ EL.Type id ty') : processed) rest

                    EV.Declaration (EV.Let id expr) -> do
                        tvar <- Shared.fresh ET.Unification
                        let qt = ET.Polymorphic $ Forall [] (Q.none :=> ET.Var tvar)
                        Eff.local (\e -> e{ types = Map.insert id _qt $ types e }) $ do
                            expr' <- elaborate expr
                            ev <- Shared.mkEvidence
                            Eff.tell $ CST.Equality ev (CST.Var tvar) (CST.Mono $ Shared.extract expr')
                            walk processed rest
                    EV.Procedure expr -> do
                        expr' <- elaborate expr
                        walk (EL.Annotated (EL.Procedure expr') ET.Void : processed) rest
                    -- EV.Declaration (EV.Let id expr) -> do
                    --     expr <- elaborate expr
                    --     Eff.local (\e -> e{ types = Map.insert id ty $ types e }) $ do
                    --         _f
                    --         --T.Forall _ qt@(_ :=> t) <- rigidify ty
                    --         -- CST.Implication vars as inner <- contextualize qt
                    --         -- ev <- Shared.mkEvidence
                    --         -- Eff.tell $ CST.Implication vars as $ CST.Conjunction inner $ CST.Equality ev (CST.Mono t) (CST.Mono inferred)
                    --         -- walk (Declaration (Let id (Just typeExp) k expr') : processed) rest
                    -- Declaration (Let id Nothing k expr) -> do

                    --     tvar <- Shared.fresh T.Unification
                    --     let qt = Forall [] (Q.none :=> T.Var tvar)
                    --     let scoped = Eff.local (\e -> e{ types = Map.insert id qt $ types e })
                    --     scoped $ do
                    --         (Typed expr' ty) <- infer expr
                    --         ev <- Shared.mkEvidence
                    --         Eff.tell $ CST.Equality ev (CST.Var tvar) (CST.Mono ty)
                    --         walk processed rest
                    --d -> walk (d:processed) rest







pattern FieldAccess record field <- EV.Application (EV.Raw (EV.Var ".")) [record, EV.Raw (EV.Var field)]
