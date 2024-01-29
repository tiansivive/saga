{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}


module Saga.Language.Typechecker.Elaboration.Values.Expressions where


import           Saga.Language.Typechecker.Elaboration.Monad           (Effects,
                                                                        Elaboration (..),
                                                                        Generalize (..))

import           Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.Elaborated.AST                   as EL

import qualified Saga.Language.Syntax.Elaborated.Kinds                 as EK
import qualified Saga.Language.Syntax.Elaborated.Types                 as ET
import qualified Saga.Language.Syntax.Elaborated.Values                as EL

import qualified Saga.Language.Syntax.Evaluated.AST                    as EV
import qualified Saga.Language.Syntax.Evaluated.Types                  as EVT
import qualified Saga.Language.Syntax.Evaluated.Values                 as EV

import           Control.Monad                                         (forM,
                                                                        forM_)
import           Control.Monad.Except                                  (ExceptT)

import           Data.Foldable                                         (for_)
import qualified Data.Map                                              as Map
import           Effectful                                             (Eff)
import qualified Effectful.Error.Static                                as Eff
import qualified Effectful.Reader.Static                               as Eff
import qualified Effectful.State.Static.Local                          as Eff
import qualified Effectful.Writer.Static.Local                         as Eff
import qualified Saga.Language.Syntax.Polymorphism                     as Q
import           Saga.Language.Syntax.Polymorphism                     (Polymorphic (..),
                                                                        Qualified (..))
import           Saga.Language.Typechecker.Elaboration.Generalization
import           Saga.Language.Typechecker.Elaboration.Types.Types
import qualified Saga.Language.Typechecker.Elaboration.Values.Effects  as Effs
import           Saga.Language.Typechecker.Elaboration.Values.Effects  (State (..))
import           Saga.Language.Typechecker.Elaboration.Values.Patterns
import qualified Saga.Language.Typechecker.Elaboration.Values.Shared   as Shared

import           Saga.Language.Typechecker.Env                         (CompilerState (..),
                                                                        Proofs (..))
import           Saga.Language.Typechecker.Errors                      (Exception (NotYetImplemented),
                                                                        SagaError (..),
                                                                        crash)
import qualified Saga.Language.Typechecker.Lib                         as Lib
import           Saga.Language.Typechecker.Lib                         (listConstructor)
import qualified Saga.Language.Typechecker.Solving.Constraints         as CST
import qualified Saga.Language.Typechecker.Variables                   as Var
import           Saga.Language.Typechecker.Variables                   (Variable)
import           Saga.Utils.Operators                                  ((||>))


instance Elaboration Expression where
    type Effects Expression es = Effs.Elaboration es
    elaborate (EV.Raw node) = case node of
        EV.Literal lit -> return $ EL.Annotated (EL.Literal lit) (Shared.decorate $ ET.Singleton lit)

        EV.Tuple elems ->  do
            elems' <- forM elems elaborate
            let tys = fmap EL.annotation elems'
            return $ EL.Annotated (EL.Tuple elems') (Shared.decorate $ ET.Tuple tys)

        EV.Record pairs ->  do
            pairs' <- forM pairs $ mapM elaborate
            let tys = fmap EL.annotation <$> pairs'
            return $ EL.Annotated (EL.Record pairs') (Shared.decorate $ ET.Record tys)

        EV.List elems -> do
            tvar <- Shared.fresh ET.Unification
            elems' <- forM elems elaborate
            let tys = fmap Shared.extract elems'

            forM_ tys $ \t -> do
                ev <- Shared.mkEvidence
                Eff.tell $ CST.Equality ev (CST.Var tvar) (CST.Mono t)

            list' <- elaborate $ EV.Raw Lib.listConstructor
            return $ EL.Annotated (EL.List elems') (Shared.decorate $ ET.Applied list' (Shared.decorate $ ET.Var tvar))



        EV.Lambda ps@(param : rest) body -> do
            tvar <- ET.Var <$> Shared.fresh ET.Unification
            let scoped = Eff.local (\e -> e { types = Map.insert param (Shared.decorate tvar) $ types e })
            scoped $ do
                out' <- elaborate out

                let ty = tvar `ET.Arrow` Shared.extract out'
                let expr = EL.Lambda ps out'
                return $ EL.Annotated expr (Shared.decorate ty)

            where
                out = case rest of
                    [] -> body
                    _  -> EV.Raw $ EV.Lambda rest body

        FieldAccess record field -> do
            fieldType <- ET.Var <$> Shared.fresh ET.Unification
            evidence <- Shared.mkEvidence
            record' <- elaborate record
            -- | TODO: Add row polymorphism: use a new, different constraint type
            Eff.tell $ CST.Equality evidence (CST.Mono $ Shared.extract record') (CST.Mono $ ET.Record [(field, Shared.decorate fieldType)])
            return $ EL.Annotated (EL.FieldAccess record' field) (Shared.decorate fieldType)

        EV.Application (EV.Raw (EV.Var ".")) args -> Eff.throwError $ Fail $ "Unrecognised expressions for property access:\n\t" ++ show args
        app'@(EV.Application fn [arg]) -> do
            out <- ET.Var <$> Shared.fresh ET.Unification
            fn'   <- elaborate fn
            arg'  <- elaborate arg

            -- | TODO: How can we notify the constraint solver that there's a new unification variable
            -- | which stands for the result of this function application
            -- | EDIT: Do we even need to do that?
            inferred <- generalize' $ EL.annotation arg' `ET.Arrow` Shared.decorate out
            evidence <- Shared.mkEvidence
            Eff.tell $ CST.Equality evidence (Shared.toItem $ Shared.extract fn') (CST.Poly inferred)

            return $ EL.Annotated (EL.Application fn' [arg']) (Shared.decorate out)
            where

                generalize' ty = do
                    count <- Eff.gets tvars
                    (inferred, count') <- Eff.runState count $ generalize ty
                    Eff.modify $ \s -> s { tvars = count' }
                    return inferred

        EV.Application fn (a : as) -> elaborate curried
            where
                partial = EV.Raw $ EV.Application fn [a]
                curried = foldl (\f a -> EV.Raw $ EV.Application f [a]) partial as

        EV.Match scrutinee cases -> do
            scrutinee' <- elaborate scrutinee
            let ty = Shared.extract scrutinee'

            Eff.local @(CompilerState Elaborated) (\e -> let extra' = extra e in e { extra = extra' { scrutinee = ty } }) $ do
                cases' <- forM cases elaborate
                let (tvars, tys) = foldl separate ([], []) cases'
                let ty' = case length tys of
                        0 -> Nothing
                        _ -> Just . ET.Union $ fmap Shared.decorate tys

                for_ ty' $ \t -> do
                    ev <- Shared.mkEvidence
                    Eff.tell $ CST.Equality ev (Shared.toItem ty) (Shared.toItem t)

                forM_ tvars $ \v -> do
                    ev <- Shared.mkEvidence
                    Eff.tell $ CST.Equality ev (CST.Var v) (maybe (Shared.toItem ty) Shared.toItem ty')

                let out = ET.Union $ fmap (\(EL.Annotated _ ann) -> ann) cases'
                return $ EL.Annotated (EL.Match scrutinee' cases') (Shared.decorate out)

            where
                separate :: ([Variable ET.Type], [ET.Type]) -> AST Elaborated (Case Expression) -> ([Variable ET.Type], [ET.Type])
                separate (tvars, tys) caseExpr = case Shared.extract caseExpr of
                    ty@(ET.Var tvar) -> (tvar:tvars, tys)
                    ty               -> (tvars, ty:tys)


        EV.Block stmts -> walk [] stmts
            where
                walk processed [] = return $ EL.Annotated (EL.Block $ reverse processed) returnTy
                    where returnTy = case head processed of
                            EL.Annotated (EL.Return {}) ty -> ty
                            _                              -> Shared.decorate ET.Void
                walk processed ((EV.Raw stmt) : rest) = case stmt of

                    EV.Return expr                 -> do
                        annotated@(EL.Annotated _ ty) <- elaborate expr
                        walk (EL.Annotated (EL.Return annotated) ty : processed) []

                    EV.Declaration (EV.Type id ty) -> do
                        ty' <- elaborate ty
                        Eff.local (\e -> e{ types = Map.insert id ty' $ types e }) $ do
                            walk (EL.Raw (EL.Declaration $ EL.Type id ty') : processed) rest

                    EV.Declaration (EV.Let id expr) -> do
                        tvar <- Shared.fresh ET.Unification

                        Eff.local (\e -> e{ types = Map.insert id (Shared.decorate $ ET.Var tvar) $ types e }) $ do
                            expr' <- elaborate expr
                            ev <- Shared.mkEvidence
                            Eff.tell $ CST.Equality ev (CST.Var tvar) (CST.Mono $ Shared.extract expr')
                            walk processed rest
                    EV.Procedure expr -> do
                        expr' <- elaborate expr
                        walk (EL.Annotated (EL.Procedure expr') (Shared.decorate ET.Void) : processed) rest

                    d -> crash $ NotYetImplemented $ "Elaboration of block statement: " ++ show d

        EV.Var name -> do
            ty <- Shared.lookup name
            implication@(CST.Implication _ assumptions _) <- Shared.contextualize $ EL.node ty
            Eff.tell implication

            let e' = foldl elaborate' (EL.Var $ EL.Identifier name) assumptions
            return $ EL.Annotated e' ty

            where
                elaborate' expr (CST.Assume c)
                    -- | QUESTION: Do we need to also annotate types here or de we expand it during Zonking?
                    | CST.Impl (CST.Evidence e) _ protocol <- c = EL.Application (EL.Raw expr) [EL.Raw (EL.Var $ EL.Evidence e)]
                    | otherwise                                 = expr


instance Elaboration (Case Expression) where
    type Effects (Case Expression) es = Effs.Elaboration es
    elaborate (EV.Raw (EV.Case pat expr))  = Eff.local @Var.Level (+1) $ do
        (pat', tyvars) <- Eff.runWriter @TypeVars $ elaborate pat

        let (pairs, tvars) = unzip $ tyvars ||> fmap (\(id, tvar) -> ((id, Shared.decorate $ ET.Polymorphic (Forall [tvar] (ET.Var tvar))), tvar))
        narrowed <- ET.Var <$> Shared.fresh ET.Unification

        Eff.local @(CompilerState Elaborated) (\e -> let extra' = extra e in e {
            types = Map.fromList pairs <> types e,
            extra = extra' { narrowings = Map.insert (scrutinee extra') narrowed $ narrowings extra' }
        }) $ do
            (expr', constraint) <- Eff.listen @CST.Constraint $ elaborate expr
            ev <- Shared.mkEvidence
            -- TODO: Skolemize the scrutinee type and the inferred pattern tvars
            -- QUESTION: Perhaps we should add another type of constraint here, to specifically prove a refinement rather than relying on equality
            Eff.tell $ CST.Implication tvars [assume ev (Shared.extract pat') narrowed] constraint
            return $ EL.Annotated (EL.Case pat' expr') (EL.annotation expr')
            where
                assume ev ty ty' = CST.Assume $ CST.Equality ev (Shared.toItem ty) (Shared.toItem ty')


pattern FieldAccess record field <- EV.Application (EV.Raw (EV.Var ".")) [record, EV.Raw (EV.Var field)]


