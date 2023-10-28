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
import           Control.Monad.Trans.Writer                      (Writer,
                                                                  WriterT (runWriterT),
                                                                  tell)

import qualified Data.Map                                        as Map

import           Saga.Language.Typechecker.Environment           (CompilerState (..))
import           Saga.Language.Typechecker.Errors                (SagaError (..))
import qualified Saga.Language.Typechecker.Inference.Type.Shared as Shared
import           Saga.Language.Typechecker.Inference.Type.Shared (propagate)
import qualified Saga.Language.Typechecker.Lib                   as Lib
import           Saga.Language.Typechecker.Qualification         (Qualified (..))
import           Saga.Language.Typechecker.Solver.Constraints    (Constraint (Equality))

import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (DataType (..),
                                                                  Scheme (..),
                                                                  Tag (..),
                                                                  Type)
import           Saga.Utils.Operators                            ((||>))




type instance VarType Pattern I.Evidence       = CST.Evidence
type instance VarType Pattern I.Unification    = Var.PolymorphicVar Type
type instance VarType Pattern I.Skolem         = Var.PolymorphicVar Type
type instance VarType Pattern I.PolyVar        = Var.PolymorphicVar Type
type instance VarType Pattern I.Instantiation  = Var.PolymorphicVar Type

type TypeVars = [(String, PolymorphicVar Type)]
type PatternInference m a = WriterT TypeVars m a


infer :: forall m. (Shared.TypeInference m, Instantiate Type) => Pattern -> PatternInference m Type

infer Wildcard = T.Var <$> fresh U

infer (Id id) = do
    tVar <- fresh U
    emit (id, tVar)
    return $ T.Var tVar

infer (Lit l) = return $ T.Singleton l
infer (PatTuple pats rest) = T.Tuple <$> mapM infer pats

infer (PatList pats rest) = do
    tys <- mapM infer pats
    tvar <- fresh U
    let result = T.Applied Lib.listConstructor $ choice (T.Var tvar) tys

    case rest of
      Nothing -> return result
      Just id -> do
        tvarList <- fresh U
        emit (id, tvarList)
        ev <- fresh E
        lift $ emit' $ Equality ev (CST.Mono result) (CST.Mono $ T.Var tvarList)
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
        tvarRecord <- fresh U
        emit (id, tvarRecord)
        return result

    where
      infer' (id, Just pat) = do
            ty <- infer pat
            return (id, ty)
      infer' (id, Nothing) = do
            tvar <- fresh U
            emit (id, tvar)
            return (id, T.Var tvar)

infer (PatData tag pats) = do
    env@(Saga { tags }) <- ask

    let cons = tags ||> filter (\T.Constructor { name } -> name == tag)
    case cons of
        [T.Constructor { name, constructor, package, target }] -> do
            tys <- mapM infer pats

            cs :=> target' <- inst $ definition target

            unificationVars <- mapM (\tv -> T.Var <$> fresh U) (ftv constructor)
            Forall [] (cs' :=> package'    ) <- lift $ package     `instantiateWith` unificationVars
            Forall [] (cs' :=> constructor') <- lift $ constructor `instantiateWith` unificationVars

            forM_ (cs ++ cs') (lift . propagate)

            e1 <- fresh E
            lift $ emit' $ Equality e1 (CST.Mono $ T.Tuple tys) (CST.Mono package')
            e2 <- fresh E
            lift $ emit' $ Equality e2 (CST.Mono $ returnType constructor') (CST.Mono target')

            return target'

        [] -> throwError $ TagNotConstructor tag
        multiple -> throwError $ MultipleTagConstructors multiple

    where
        ftv (Forall tvars _) = tvars

        returnType (T.Arrow _ t) = returnType t
        returnType t             = t

        inst ty@(Forall [] qt) = return qt
        inst ty = do
            tvar  <- fresh U
            ty'  <- lift $ instantiate ty (T.Var tvar)
            inst ty'


fresh :: forall m a. Shared.TypeInference m => I.Tag a -> PatternInference m (VarType Expr a)
fresh = lift . Shared.fresh

emit :: Shared.TypeInference m => (String, PolymorphicVar Type) -> PatternInference m ()
emit pair = tell [pair]

run :: Shared.TypeInference m => PatternInference m a -> m (a, TypeVars)
run pi = runWriterT pi
