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
import           Saga.Language.Typechecker.Variables             (PolymorphicVar,
                                                                  VarType)

import           Control.Monad.Except                            (MonadError (..))
import           Control.Monad.RWS                               (MonadReader (..),
                                                                  MonadTrans (..),
                                                                  MonadWriter,
                                                                  forM, forM_)


import qualified Data.Map                                        as Map

import           Saga.Language.Typechecker.Environment           (CompilerState (..))
import           Saga.Language.Typechecker.Errors                (SagaError (..))
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




type instance VarType Pattern I.Evidence       = CST.Evidence
type instance VarType Pattern I.Unification    = Var.PolymorphicVar Type
type instance VarType Pattern I.Skolem         = Var.PolymorphicVar Type
type instance VarType Pattern I.TypeVar        = Var.PolymorphicVar Type
type instance VarType Pattern I.Instantiation  = Var.PolymorphicVar Type


type PatternInferenceEff es = (InferEff es CST.Constraint, Eff.Writer TypeVars :> es)

type TypeVars = [(String, PolymorphicVar Type)]



infer :: (PatternInferenceEff es, Instantiate Type) => Pattern -> Eff es Type

infer Wildcard = T.Var <$> fresh U

infer (Id id) = do
    uvar <- fresh U
    emit (id, uvar)
    return $ T.Var uvar

infer (Lit l) = return $ T.Singleton l
infer (PatTuple pats rest) = T.Tuple <$> mapM infer pats

infer (PatList pats rest) = do
    tys <- mapM infer pats
    tvar <- fresh U
    let result = T.Applied Lib.listConstructor $ choice (T.Var tvar) tys

    case rest of
      Nothing -> return result
      Just id -> do
        l <- Eff.gets level
        uvarList <- fresh U
        emit ( id, uvarList)
        ev <- fresh E
        Eff.tell $ Equality ev (CST.Mono result) (CST.Variable (CST.Level l) uvarList)
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
        uvarRecord <- fresh U
        emit ( id, uvarRecord)
        return result

    where
      infer' (id, Just pat) = do
            ty <- infer pat
            return (id, ty)
      infer' (id, Nothing) = do
            uvar <- fresh U
            emit ( id, uvar)
            return (id, T.Var uvar)

infer (PatData tag pats) = do
    env@(Saga { tags }) <- Eff.ask

    let cons = tags ||> filter (\T.Constructor { name } -> name == tag)
    case cons of
        [T.Constructor { name, constructor, package, target }] -> do
            tys <- mapM infer pats

            bs :| cs :=> target' <- inst $ definition target

            unificationVars <- mapM (\tv -> T.Var <$> fresh U) (ftv constructor)
            Forall [] (bs' :| cs' :=> package'    ) <- package     `instantiateWith` unificationVars
            Forall [] (bs' :| cs' :=> constructor') <- constructor `instantiateWith` unificationVars

            forM_ (cs <> cs') propagate

            e1 <- fresh E
            Eff.tell $ Equality e1 (CST.Mono $ T.Tuple tys) (CST.Mono package')
            e2 <- fresh E
            Eff.tell $ Equality e2 (CST.Mono $ returnType constructor') (CST.Mono target')

            return target'

        [] -> Eff.throwError $ TagNotConstructor tag
        multiple -> Eff.throwError $ MultipleTagConstructors multiple

    where
        ftv (Forall tvars _) = tvars

        returnType (T.Arrow _ t) = returnType t
        returnType t             = t

        inst ty@(Forall [] qt) = return qt
        inst ty = do
            uvar  <- fresh U
            inst $ instantiate ty (T.Var uvar)


fresh ::  PatternInferenceEff es =>I.Tag a -> Eff es (VarType Expr a)
fresh = Shared.fresh

emit :: PatternInferenceEff es => (String, PolymorphicVar Type) -> Eff es ()
emit pair = Eff.tell [pair]

--run :: PatternInference a -> m (a, TypeVars)





-- run ::PatternInference a -> Shared.TypeInference (a, TypeVars)
-- run = Eff.runWriter . Eff.inject


    --  Eff.inject $ Eff.runWriter pi
