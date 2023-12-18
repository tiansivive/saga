{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}



module Saga.Language.Typechecker.Inference.Type.Pattern where
import           Saga.Language.Core.Expr                         (Expr,
                                                                  Pattern (..))
import qualified Saga.Language.Typechecker.Inference.Inference   as I hiding
                                                                      (emit,
                                                                       fresh,
                                                                       infer)
import           Saga.Language.Typechecker.Inference.Inference   hiding (emit,
                                                                  fresh, infer)
import qualified Saga.Language.Typechecker.Solver.Constraints    as CST
import qualified Saga.Language.Typechecker.Variables             as Var
import           Saga.Language.Typechecker.Variables             (VarType,
                                                                  Variable)

import           Control.Monad.Except                            (MonadError (..))
import           Control.Monad.RWS                               (MonadReader (..),
                                                                  MonadTrans (..),
                                                                  MonadWriter,
                                                                  forM, forM_)


import qualified Data.Map                                        as Map

import           Saga.Language.Typechecker.Environment           (CompilerState (..))
import           Saga.Language.Typechecker.Errors                (Exception (NotYetImplemented),
                                                                  SagaError (..),
                                                                  crash)
import qualified Saga.Language.Typechecker.Inference.Type.Shared as Shared
import           Saga.Language.Typechecker.Inference.Type.Shared (propagate)
import qualified Saga.Language.Typechecker.Lib                   as Lib
import           Saga.Language.Typechecker.Qualification         (Given (..),
                                                                  Qualified (..))
import           Saga.Language.Typechecker.Solver.Constraints    (Constraint (Equality))

import           Effectful                                       (Eff, (:>))
import qualified Effectful                                       as Eff
import qualified Effectful.Error.Static                          as Eff
import           Effectful.Reader.Static                         (Reader)
import qualified Effectful.Reader.Static                         as Eff
import qualified Effectful.State.Static.Local                    as Eff
import qualified Effectful.Writer.Static.Local                   as Eff
import qualified Saga.Language.Core.Expr                         as E
import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (DataType (..),
                                                                  Scheme (..),
                                                                  Tag (..),
                                                                  Type)
import           Saga.Utils.Operators                            ((||>))






type PatternInferenceEff es = (Shared.TypeInference es, Eff.Writer TypeVars :> es)

type TypeVars = [(String, Variable Type)]



infer :: (PatternInferenceEff es, Instantiate Type) => Pattern -> Eff es Type

infer Wildcard = T.Var <$> Shared.fresh

infer (Id id) = do
    tvar <- Shared.fresh
    emit (id, tvar)
    return $ T.Var tvar

infer (Lit l) = return $ T.Singleton l
infer (PatTuple pats rest) = T.Tuple <$> mapM infer pats

infer (PatList pats rest) = do
    tys <- mapM infer pats
    tvar <- Shared.fresh

    let result = T.Applied Lib.listConstructor $ choice (T.Var tvar) tys

    case rest of
      Nothing -> return result
      Just id -> do
        tvar <- Shared.fresh
        ev   <- Shared.mkEvidence
        Eff.tell $ Equality ev (CST.Mono result) (CST.Unification  tvar)
        emit (id, tvar)
        return result

    where
      choice tvar []    = tvar
      choice tvar (t:_) = t

infer (PatRecord pairs rest) = do
    inferred <- forM pairs infer'
    let result = T.Record inferred
    case rest of
      Nothing -> return result
      Just id -> do
        tvar <- Shared.fresh
        emit (id, tvar)
        return result

    where
      infer' (id, Just pat) = do
            ty <- infer pat
            return (id, ty)
      infer' (id, Nothing) = do
            tvar <- Shared.fresh
            emit ( id, tvar)
            return (id, T.Var tvar)

infer (PatData tag pats) = crash $ NotYetImplemented "PatData inference will change as data types will be removed"
    -- do
    -- env@(Saga { tags }) <- Eff.ask

    -- let cons = tags ||> filter (\T.Constructor { name } -> name == tag)
    -- case cons of
    --     [T.Constructor { name, constructor, package, target }] -> do
    --         tys <- mapM infer pats

    --         bs :| cs :=> target' <- inst $ definition target

    --         unificationVars <- mapM (\tv -> T.Var <$> Shared.fresh) (ftv constructor)
    --         Forall [] (bs' :| cs' :=> package'    ) <- package     `instantiateWith` unificationVars
    --         Forall [] (bs' :| cs' :=> constructor') <- constructor `instantiateWith` unificationVars

    --         forM_ (cs <> cs') propagate

    --         e1 <- fresh E
    --         Eff.tell $ Equality e1 (CST.Mono $ T.Tuple tys) (CST.Mono package')
    --         e2 <- fresh E
    --         Eff.tell $ Equality e2 (CST.Mono $ returnType constructor') (CST.Mono target')

    --         return target'

    --     [] -> Eff.throwError $ TagNotConstructor tag
    --     multiple -> Eff.throwError $ MultipleTagConstructors multiple

    -- where
    --     ftv (Forall tvars _) = tvars

    --     returnType (T.Arrow _ t) = returnType t
    --     returnType t             = t

    --     inst ty@(Forall [] qt) = return qt
    --     inst ty = do
    --         uvar  <- fresh U
    --         inst $ instantiate ty (T.Var uvar)



emit :: PatternInferenceEff es => (String, Variable Type) -> Eff es ()
emit pair = Eff.tell [pair]

