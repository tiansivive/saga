{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


import           Saga.Language.Typechecker.Elaboration.Monad          (Effects,
                                                                       Elaboration (..))

import           Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL

import qualified Saga.Language.Syntax.Elaborated.Types                as ET
import qualified Saga.Language.Syntax.Elaborated.Values               as EL

import           Control.Monad                                        (forM)
import           Effectful                                            (Eff,
                                                                       (:>))
import qualified Effectful.Writer.Static.Local                        as Eff
import qualified Saga.Language.Syntax.Evaluated.AST                   as EV
import qualified Saga.Language.Syntax.Evaluated.Values                as EV
import qualified Saga.Language.Typechecker.Elaboration.Values.Effects as Effs
import qualified Saga.Language.Typechecker.Elaboration.Values.Shared  as Shared
import qualified Saga.Language.Typechecker.Lib                        as Lib
import qualified Saga.Language.Typechecker.Solver.Constraints         as CST
import           Saga.Language.Typechecker.Variables                  (Variable)


type PatternEff es = (Effs.Elaboration es, Eff.Writer TypeVars :> es)

type TypeVars = [(String, Variable ET.Type)]

instance Elaboration (Pattern Expression) where
    type Effects (Pattern Expression) es = PatternEff es

    elaborate (EV.Raw pat) = case pat of
        EV.Wildcard -> do
            ty <- ET.Var <$> Shared.fresh ET.Unification
            return $ EL.Annotated EL.Wildcard ty
        EV.Id id -> do
            tvar <- Shared.fresh ET.Unification
            emit (id, tvar)
            return $ EL.Annotated (EL.Id id) (ET.Var tvar)

        EV.PatLit l -> return $ EL.Annotated (EL.PatLit l) (ET.Singleton l)
        EV.PatTuple pats rest -> do
            pats' <- mapM elaborate pats
            return $ EL.Annotated (EL.PatTuple pats' rest) (ET.Tuple $ fmap Shared.extract pats')
        EV.PatList pats rest -> do
            pats' <- mapM elaborate pats
            tvar <- Shared.fresh ET.Unification
            let result = EL.Annotated (EL.PatList pats' rest) (ET.Applied _Lib.listConstructor $ choice (ET.Var tvar) pats')

            case rest of
                Nothing -> return result
                Just id -> do
                    tvar <- Shared.fresh ET.Unification
                    ev   <- Shared.mkEvidence
                    Eff.tell $ CST.Equality ev (CST.Mono result) (CST.Var tvar)
                    emit (id, tvar)
                    return result

            where
                choice tvar []    = tvar
                choice tvar (t:_) = Shared.extract t

        EV.PatRecord pairs rest -> do
            pairs' <- forM pairs elaborate'
            let result = EL.Annotated (EL.PatRecord pairs' rest) (ET.Record $ fmap (fmap Shared.extract) pairs')
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
                        return (id, EL.Annotated (EL.Id id) (ET.Var tvar))

    


emit :: PatternEff es => (String, Variable ET.Type) -> Eff es ()
emit pair = Eff.tell [pair]

