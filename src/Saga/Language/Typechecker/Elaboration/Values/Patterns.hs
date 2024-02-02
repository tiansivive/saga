{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Elaboration.Values.Patterns where

import           Saga.Language.Typechecker.Elaboration.Monad         (Effects,
                                                                      Elaboration (..))

import           Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.Elaborated.AST                 as EL

import qualified Saga.Language.Syntax.Elaborated.Types               as ET
import qualified Saga.Language.Syntax.Elaborated.Values              as EL

import           Control.Monad                                       (forM)
import           Effectful                                           (Eff, (:>))
import qualified Effectful.Writer.Static.Local                       as Eff
import qualified Saga.Language.Syntax.Reduced.AST                    as RD
import qualified Saga.Language.Syntax.Reduced.Values                 as RD
import qualified Saga.Language.Typechecker.Elaboration.Effects       as Effs
import qualified Saga.Language.Typechecker.Elaboration.Values.Shared as Shared
import qualified Saga.Language.Typechecker.Lib                       as Lib
import qualified Saga.Language.Typechecker.Solving.Constraints       as Solver
import           Saga.Language.Typechecker.Variables                 (Variable)

import           Saga.Language.Typechecker.Elaboration.Types.Types

import           Saga.Utils.Common                                   (fmap2)


type PatternEff es = (Effs.Elaboration es, Eff.Writer TypeVars :> es)

type TypeVars = [(String, Variable ET.Type)]


instance Elaboration (Pattern Expression) where
    type Effects (Pattern Expression) es = PatternEff es

    elaborate (RD.Raw pat) = case pat of
        RD.Wildcard -> do
            ty <- ET.Var <$> Shared.fresh ET.Unification
            return $ EL.Annotated EL.Wildcard (Shared.decorate ty)
        RD.Id id -> do
            tvar <- Shared.fresh ET.Unification
            emit (id, tvar)
            return $ EL.Annotated (EL.Id id) (Shared.decorate $ ET.Var tvar)

        RD.PatLit l -> return $ EL.Annotated (EL.PatLit l) (Shared.decorate $ ET.Singleton l)
        RD.PatTuple pats rest -> do
            pats' <- mapM elaborate pats
            return $ EL.Annotated (EL.PatTuple pats' rest) (Shared.decorate . ET.Tuple $ fmap EL.annotation pats')
        RD.PatList pats rest -> do
            pats' <- mapM elaborate pats
            tvar <- Shared.fresh ET.Unification

            let result = EL.Annotated (EL.PatList pats' rest) (list' $ choice (ET.Var tvar) pats')

            case rest of
                Nothing -> return result
                Just id -> do
                    tvar <- Shared.fresh ET.Unification
                    ev   <- Shared.mkEvidence
                    Eff.tell $ Solver.Equality ev (Solver.Ty $ Shared.extract result) (Solver.Ty $ ET.Var tvar)
                    emit (id, tvar)
                    return result

            where
                choice tvar []    = Shared.decorate tvar
                choice tvar (t:_) = EL.annotation t
                list' = Shared.decorate . ET.Applied (EL.Raw Lib.listConstructor)

        RD.PatRecord pairs rest -> do
            pairs' <- forM pairs elaborate'
            let result = EL.Annotated (EL.PatRecord pairs' rest) (Shared.decorate . ET.Record $ fmap2 EL.annotation pairs')
            case rest of
                Nothing -> return result
                Just id -> do
                    tvar <- Shared.fresh ET.Unification
                    emit (id, tvar)
                    return result

            where
                elaborate' (id, Just pat) = do
                        pat' <- elaborate pat
                        return (id, pat')
                elaborate' (id, Nothing) = do
                        tvar <- Shared.fresh ET.Unification
                        emit ( id, tvar)
                        return (id, EL.Annotated (EL.Id id) (Shared.decorate $ ET.Var tvar))




emit :: PatternEff es => (String, Variable ET.Type) -> Eff es ()
emit pair = Eff.tell [pair]

