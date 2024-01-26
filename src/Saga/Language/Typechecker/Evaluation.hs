{-# LANGUAGE DataKinds           #-}

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module Saga.Language.Typechecker.Evaluation where
import           Control.Monad.RWS



import           Control.Applicative                           ((<|>))
import           Control.Monad.Except
import           Control.Monad.Trans.Reader                    (ReaderT (runReaderT))
import           Control.Monad.Trans.Writer                    (WriterT (runWriterT))
import           Data.Functor                                  ((<&>))
import qualified Data.List                                     as List
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Debug.Trace                                   (traceM)
import           Effectful                                     (Eff, (:>))
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Prelude                                       hiding (id,
                                                                lookup)
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (Exception (NotYetImplemented),
                                                                SagaError (..),
                                                                crash)

import           Saga.Language.Typechecker.Monad               (TypeCheck)
import           Saga.Language.Typechecker.Protocols           (Protocol (..))



import qualified Effectful                                     as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Saga.Language.Typechecker.Inference.Inference as I
import qualified Saga.Language.Typechecker.Inference.Kind      as KI

import           Saga.Language.Typechecker.Inference.Kind      (kind)

import           Saga.Language.Syntax.Desugared.Types          (Node, TypeExpr)
import           Saga.Language.Syntax.Polymorphism             (Given (..),
                                                                Polymorphic (..),
                                                                Qualified (..))
import           Saga.Language.Typechecker.Solver.Substitution (ftv)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (Classifier,
                                                                Variable)
import           Saga.Utils.Operators                          ((|$>), (|>),
                                                                (|>:))

import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      hiding
                                                               (NodeType (..))
import qualified Saga.Language.Syntax.Desugared.Types          as TE
import qualified Saga.Language.Syntax.Evaluated.AST            as T
import qualified Saga.Language.Syntax.Evaluated.Kinds          as K
import qualified Saga.Language.Syntax.Evaluated.Types          as T
import           Saga.Language.Syntax.Evaluated.Types          (Type)
import qualified Saga.Language.Syntax.Polymorphism             as T
import           Saga.Language.Typechecker.Evaluation.Shared   (node)
import           Saga.Utils.Common                             (forM2)



type EvaluationEff es = TypeCheck es
type EvaluationM a = forall es. (EvaluationEff es) => Eff es a

class Evaluate a where
    evaluate :: EvaluationEff es => Node Desugared a -> Eff es (Node Evaluated a)

instance Evaluate NT.Type where

    evaluate (TE.Identifier id)  = lookup id

    evaluate (TE.Arrow t1 t2) = do
        t1' <- evaluate t1
        t2' <- evaluate t2
        return $ t1' `T.Arrow` t2'
    -- | TODO: need to probably run kind inference first
    --evaluate (TE.Tagged tag tyExp) = evaluate tyExp <&> \(Forall tvars (given :=> qt)) -> Forall tvars (given :=> T.Data tag (K.Var $ K.Poly  tag K.Kind) `T.Applied` qt)
    evaluate (TE.Clause tyExpr bindings)      = do
        ty' <- evaluate tyExpr

        bindings' <- binds
        return $ T.Qualified (bindings' :| constraints :=> ty')

        where
            locals = [ (id, ty) | TE.Bind id ty <- bindings]
            binds = fmap Map.fromList $ forM locals $ \(id, ty) -> evaluate ty >>= \t -> return (T.Local id K.Type, t)

            constraints = do
                TE.Constraint (TE.Identifier id `TE.Implements` protocol) <- bindings
                return $ T.Var (T.Poly  id $ K.Var (K.Poly id K.Kind)) `T.Implements` protocol

    evaluate (TE.Implementation prtcl tyExpr) = do
        Saga {protocols} <- Eff.ask
        case List.find (\(Protocol { id }) -> id == prtcl) protocols of
            Nothing                -> error $ "Could not find Protocol: " ++ prtcl
            Just (Protocol { spec }) -> evaluate $ TE.Application spec [tyExpr]

    evaluate (TE.Lambda params body) = closure body params

    -- | TYPE APPLICATIONS
    evaluate (TE.Application (TE.Identifier ".") [record,  TE.Identifier field]) = do
        record' <- evaluate record
        case record' of
            T.Record pairs -> maybe (Eff.throwError $ not_field record') (return . node) (List.lookup field pairs)
            _             -> Eff.throwError $ Fail $ "Type\n\t" ++ show record' ++ "\nis not a Record when trying to access field\n\t" ++ field
        where
            not_field r = Fail $ field ++ " is not a field of " ++ show r

    evaluate (TE.Application fnExpr argExprs) = do
        fn' <- evaluate fnExpr
        args <- mapM evaluate argExprs

        case fn' of
            -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here

            t@(T.Data {}) -> return . node $ application t args
            -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here
            t@(T.Applied {}) -> return . node $ application t args
            T.Closure params closure env -> apply closure params args
            T.Var (T.Poly  t _) -> do
                T.Raw t' <- lookup t
                case t' of
                    -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here
                    T.Data tycon k -> return . node $ application t' args
                    -- | TODO: #21 @tiansivive :Kinds:TypeEvaluation we probably need some kind check here
                    T.Applied cons arg -> return . node $ application t' args
                    T.Closure params closure env -> apply closure params args
                    _ -> Eff.throwError $ UnexpectedType t' "Cannot apply this variable expression"
            _ -> Eff.throwError $ UnexpectedType fn' "Cannot apply this type expression"

        where
            application (T.Raw -> t) (fmap T.Raw -> args) = foldl (T.Applied |>: T.Raw ) t args
                -- let
                --     (bs, cs, tvars, tys) = collect args
                --     applied = foldl T.Applied t tys
                -- in return . T.Polymorphic $ Forall tvars (bs :| cs :=> _applied)

            apply :: TypeExpr -> [Variable Type] -> [Type] -> EvaluationM Type
            apply tyExpr [] [] = evaluate tyExpr
            apply tyExpr tvars [] = do
                Saga { types, kinds } <- Eff.ask
                return $ T.Closure tvars tyExpr (T.Scope types kinds)

            apply tyExpr (p@(T.Poly id k) :params) (a :args) = Eff.local (\e -> e{ types = Map.insert id (T.Raw $ T.Var p) (types e) }) $ do
                apply tyExpr params args
            apply _ [] (a: args) = Eff.throwError $ TooManyArguments fnExpr argExprs

    evaluate (TE.Match t cases)  = error "Evaluation of Match type expressions not implemented yet"

    evaluate atom  = case atom of
        TE.Union tys -> T.Union <$> forM tys (evaluate |$> T.Raw)
        TE.Tuple tys -> T.Tuple <$> forM tys (evaluate |$> T.Raw)
        TE.Record pairs -> do
            pairs' <- forM2 pairs $ evaluate |$> T.Raw
            return $ T.Record pairs'
        atom  -> crash $ NotYetImplemented $ "Evaluation of type atom: " ++ show atom






lookup :: String -> EvaluationM (AST Evaluated NT.Type)
--lookup id | trace ("Looking up: " ++ id) False = undefined
lookup id = do
    Saga { types } <- Eff.ask
    maybe (Eff.throwError $ UndefinedIdentifier id) return $ Map.lookup id types


closure :: TypeExpr -> [String] -> EvaluationM Type
closure body params = do
    Saga { types, kinds } <- Eff.ask
    return $ T.Closure tvars body (T.Scope types kinds)

    where
        tvars = fmap (\v -> T.Poly v (K.Var $ K.Poly v K.Kind)) params

