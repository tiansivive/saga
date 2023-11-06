{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonadComprehensions    #-}
{-# LANGUAGE TypeFamilies           #-}
module Saga.Language.Typechecker.Evaluation where
import           Control.Monad.RWS

import qualified Saga.Language.Typechecker.Type                as T
import           Saga.Language.Typechecker.Type                (DataType (..),
                                                                Polymorphic,
                                                                Scheme (..),
                                                                Type)
import qualified Saga.Language.Typechecker.TypeExpr            as TE
import           Saga.Language.Typechecker.TypeExpr            (TypeExpr)

import           Control.Applicative                           ((<|>))
import           Control.Monad.Except
import           Control.Monad.Trans.Reader                    (ReaderT (runReaderT))
import           Control.Monad.Trans.Writer                    (WriterT (runWriterT))
import           Data.Functor                                  ((<&>))
import qualified Data.List                                     as List
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff, (:>))
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Prelude                                       hiding (id,
                                                                lookup)
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Kind                as K
import           Saga.Language.Typechecker.Kind                (Kind)
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import           Saga.Language.Typechecker.Protocols           (Protocol (..))
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Qualification       (Qualified ((:=>)))
import           Saga.Language.Typechecker.Solver.Substitution (ftv)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (Classifier,
                                                                PolymorphicVar)
import           Saga.Utils.Operators                          ((|>))



type EvaluationM = TypeCheck '[]

class Evaluate a b | a -> b where
    evaluate ::  a -> EvaluationM b

instance Evaluate TypeExpr (Polymorphic Type) where
    evaluate (TE.Identifier id)  = lookup id

    evaluate (TE.Arrow t1 t2) = do
        Forall tvars qIn <- evaluate t1
        Forall tvars' qOut <- evaluate t2
        return $ Forall (tvars ++ tvars') $ T.Arrow <$> qIn <*> qOut
    -- | TODO: need to probably run kind inference first
    evaluate (TE.Tagged tag tyExp) = evaluate tyExp <&> \(Forall tvars qt) -> Forall tvars $ fmap (T.Applied $ T.Data tag $ K.Var (Var.Type tag K.Kind)) qt
    evaluate (TE.Clause tyExpr bindings)      = do
        binds <- Map.fromList <$> bindGroup
        let scoped = Eff.local (\s -> s { types = binds `Map.union` types s })
        scoped $ do
            Forall tvars (cs :=> t) <- evaluate tyExpr
            return $ Forall tvars $ cs ++ constraints :=> t

        where
            bindGroup = sequence $ do
                TE.Bind id ty <- bindings
                return $ do
                    ty' <- evaluate ty
                    return (id, ty')

            constraints = do
                TE.Constraint (TE.Identifier id `Q.Implements` protocol) <- bindings
                return $ T.Var (Var.Type id $ K.Var (Var.Type id K.Kind)) `Q.Implements` protocol

    evaluate (TE.Implementation prtcl tyExpr) = do
        Saga {protocols} <- Eff.ask
        case List.find (\(Protocol { id }) -> id == prtcl) protocols of
            Nothing                -> error $ "Could not find Protocol: " ++ prtcl
            Just (Protocol { spec }) -> evaluate $ TE.Application spec [tyExpr]

    evaluate (TE.Lambda params body) = closure body params

    -- | TYPE APPLICATIONS
    evaluate (TE.Application (TE.Identifier ".") [record,  TE.Identifier field]) = do
        Forall tvars (cs :=> record') <- evaluate record
        case record' of
            T.Record pairs -> maybe (Eff.throwError $ not_field record') (return . qualified cs) (List.lookup field pairs)
            _             -> Eff.throwError $ Fail $ "Type\n\t" ++ show record' ++ "\nis not a Record when trying to access field\n\t" ++ field
        where
            qualified cs t = Forall (Set.toList $ ftv t) (filter (byType t) cs :=> t)

            byType t (Q.Implements t' _) = t == t'
            byType t (Q.Resource m t')   = t == t'
            byType t (Q.Refinement _ t') = t == t'
            byType t (Q.Pure t')         = t == t'

            not_field r = Fail $ field ++ " is not a field of " ++ show r

    evaluate (TE.Application fnExpr argExprs) = do
        Forall tvars (cs :=> qt) <- evaluate fnExpr
        args <- mapM evaluate argExprs

        case qt of
            -- | TODO:#Kinds we probably need some kind check here
            t@(T.Data {}) -> application t args (tvars, cs)
            -- | TODO:#Kinds we probably need some kind check here
            t@(T.Applied {}) -> application t args (tvars, cs)
            T.Closure params closure env -> apply closure params args
            T.Var (Var.Type t _) -> do
                Forall tvars' (cs' :=> qt') <- lookup t
                case qt' of
                    -- | TODO:#Kinds we probably need some kind check here
                    t@(T.Data tycon k) -> application t args (tvars', cs')
                    -- | TODO:#Kinds we probably need some kind check here
                    t@(T.Applied cons arg) -> application t args (tvars', cs')
                    T.Closure params closure env -> apply closure params args
                    _ -> Eff.throwError $ UnexpectedType qt' "Cannot apply this variable expression"
            _ -> Eff.throwError $ UnexpectedType qt "Cannot apply this type expression"

        where
            application t args context =
                let
                    (cs, tvars, tys) = collect args
                    applied = foldl T.Applied t tys
                in return $ Forall (tvars ++ fst context) (cs ++ snd context :=> applied)

            apply :: TypeExpr -> [PolymorphicVar Type] -> [Polymorphic Type] -> EvaluationM (Polymorphic Type)
            apply tyExpr [] [] = evaluate tyExpr
            apply tyExpr tvars [] = do
                Saga { types, dataTypes, kinds, tags } <- Eff.ask
                return $ Forall tvars $ [] :=> T.Closure tvars tyExpr (T.Scope types kinds dataTypes tags)

            apply tyExpr (p@(Var.Type id k) :params) (a: args) = do
                let scoped = Eff.local (\e -> e{ types = Map.insert id (Forall [p] ([] :=> T.Var p)) (types e) })
                scoped $ apply tyExpr params args
            apply _ [] (a: args) = Eff.throwError $ TooManyArguments fnExpr argExprs


    evaluate (TE.Match t cases)  = error "Evaluation of Match type expressions not implemented yet"

    evaluate atom  = case atom of
        TE.Union tys -> evaluate' (\(cs, tvars, ts) -> Forall tvars $ cs :=> T.Union ts) tys
        TE.Tuple tys -> evaluate' (\(cs, tvars, ts) -> Forall tvars $ cs :=> T.Tuple ts) tys
        TE.Record pairs -> do
            pairs' <- mapM (mapM evaluate) pairs
            let (cs, tvars, ps) = foldr (\(key, Forall tvars (cs' :=> qt)) (cs, tvars', ts) -> (cs' ++ cs, tvars' ++ tvars, (key, qt) : ts)) ([],[],[]) pairs'
            return $ Forall tvars $ cs :=> T.Record ps

        where
            evaluate' build = mapM evaluate |> fmap collect >=> (return . build)





lookup :: String -> EvaluationM (T.Polymorphic Type)
--lookup id | trace ("Looking up: " ++ id) False = undefined
lookup id = do
    Saga { types, dataTypes } <- Eff.ask

    let value
          =   [ definition | DataType { definition } <- Map.lookup id dataTypes]
          <|> Map.lookup id types

    maybe (Eff.throwError $ UndefinedIdentifier id) return value


collect :: [T.Polymorphic t] -> ([Q.Constraint t], [PolymorphicVar t], [t])
collect = foldr (\(Forall tvars (cs' :=> qt)) (cs, tvars', ts) -> (cs' ++ cs, tvars ++ tvars', qt : ts)) ([],[], [])


closure :: TypeExpr -> [String] -> EvaluationM (Polymorphic Type)
closure body params = do
    Saga { types, dataTypes, kinds, tags } <- Eff.ask
    return $ Forall tvars $ [] :=> T.Closure tvars body (T.Scope types kinds dataTypes tags)

    where
        tvars = fmap (\v -> Var.Type v (K.Var $ Var.Kind v K.Kind)) params

