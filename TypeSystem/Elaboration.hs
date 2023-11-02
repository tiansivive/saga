{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Saga.Language.TypeSystem.Elaboration where

import           Control.Monad.Except
import           Control.Monad.Reader                 (Reader,
                                                       ReaderT (runReaderT),
                                                       runReader)
import           Control.Monad.RWS
import           Control.Monad.State                  (State, get, runState)
import           Control.Monad.Trans.Writer           (WriterT (runWriterT),
                                                       runWriter)
import           Data.Bifunctor                       (Bifunctor (first))
import           Data.Functor                         ((<&>))
import           Data.List                            (find, intercalate,
                                                       intersect)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe
import qualified Data.Set                             as Set
import           Data.Traversable                     (for)
import           Debug.Trace                          (traceM)
import           Prelude                              hiding (id, log)
import           Saga.Language.Core.Expr
import           Saga.Language.Core.Literals          (Literal (..))
import           Saga.Language.TypeSystem.Constraints (Solve, Subst,
                                                       Substitutable (apply),
                                                       ftv, unify)
import           Saga.Language.TypeSystem.Environment (Accumulator,
                                                       CompilerState (protocols, types, values),
                                                       Protocol (Protocol, id, implementations),
                                                       ProtocolID, Saga,
                                                       Tell (Error),
                                                       UnificationVar, log)
import           Saga.Language.TypeSystem.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.Inference   hiding (log)
import           Saga.Language.TypeSystem.Lib         (defaultEnv)
import qualified Saga.Language.TypeSystem.Refinement  as Refine
import           Saga.Language.TypeSystem.Types
import           Saga.Utils.Utils                     ((|>), (||>))




type Dicts = Map.Map (Tyvar, ProtocolID) Dictionary
type Dictionary = String

type Elaboration a = Saga Dicts (Except SagaError) a


run :: CompilerState -> TypeExpr -> Expr -> Except SagaError Expr
run state (TQualified (cs :=> ty)) e = do
    (e', _, _) <- runRWST (elaborate e) dicts state
    return $ Lambda (Map.toList dicts ||> fmap snd) e'
    where
        pair (Implements t@(TVar v) p) = ((v, p), generateName t ++ "$" ++ p)
        dicts = cs ||> fmap pair |> Map.fromList


elaborateScript :: CompilerState -> Script -> Either SagaError (Script, CompilerState, Accumulator)
elaborateScript st (Script decs) = runExcept $ runRWST elaboration Map.empty st
    where
        elaboration =  Script <$> forM decs elaborateDec
elaborateDec :: Declaration -> Elaboration Declaration
elaborateDec (Let id ty k e) = Let id ty k <$> case ty of
    Just ty' -> elaborate' ty' e
    Nothing  -> elaborate e

    where
        elaborate' (TQualified (cs :=> ty)) e = do
            let dicts = cs ||> fmap pair |> Map.fromList
            let scoped = local (<> dicts)
            scoped $ Lambda (Map.toList dicts ||> fmap snd) <$> elaborate e
        elaborate' t e = elaborate e

        pair (Implements t@(TVar v) p) = ((v, p), generateName t ++ "$" ++ p)
elaborateDec d = return d



elaborate :: Expr -> Elaboration Expr
elaborate e@(Typed (Identifier id) ty') = do
    st <- get
    ty <- gets $ types |> Map.lookup id
    dictMap <- ask
    -- traceM "elaborating ID in fn app:"
    -- traceM $ "\tEnv type: " ++ show ty
    -- traceM $ "\tInferred type: " ++ show ty'
    -- traceM $ "\tDict Map: " ++ show dictMap

    case ty of
        Just (TQualified (cs :=> tyExpr)) -> do

            cs' <- either (throwError . Fail) return instConstraints

            let (tvars, tys) = foldl separate ([], []) cs'
            --traceM $ "\n Instantiated Constraints':\n\t" ++ show cs'


            dicts <- forM cs' \c@(Implements ty p) -> case ty of
                    TVar tvar -> Identifier <$> maybe (throwError $ id_not_found c) return (Map.lookup (tvar, p) dictMap)
                    _         -> case getDict (ty,p) of
                        [(_,_, record)] -> return record
                        []              -> throwError $ impl_not_found c
                        dicts           -> throwError $ multiple_impls c

            -- traceM $ "Separated':\n\t" ++ show (tvars, tys)
            -- traceM $ "Dicts':\n\t" ++ show dicts

            return $ FnApp (Identifier id) dicts
            where
                getDict (ty, p) =  [ (ty', id, record)
                                    | p'@(Protocol {id}) <- protocols st, id == p
                                    , impl@(_ :=> (ty', record)) <- implementations p', ty == ty'
                                    ]

                separate (tvars, tys) constraint = case constraint of
                    Implements (TVar tvar) p -> ((tvar, p):tvars, tys)
                    Implements ty          p -> (tvars, (ty, p):tys)

                instConstraints = do
                    t <- Refine.runIn st tyExpr
                    (sub, _) <- show `first` runExcept (runWriterT $ runReaderT (unify t ty' :: Solve Subst) st)
                    return $ apply sub cs

                id_not_found c = Fail $ "Couldn't find provided protocol dictionary identifier for " ++ show c
                impl_not_found c = Fail $ "Couldn't find protocol implementation for " ++ show c
                multiple_impls c = Fail $ "Found multiple protocol implementation for " ++ show c

        _ -> return e


elaborate (Typed e _) = elaborate e
elaborate e@(Hole {}) = return e
elaborate e@(Literal {}) = return e
elaborate e@(Identifier {}) = return e

elaborate e@(List es) = List <$> forM es elaborate
elaborate e@(Tuple es) = Tuple <$> forM es elaborate
elaborate e@(Record pairs) = Record <$> forM pairs (mapM elaborate)

elaborate e@(Match cond cases) = do
    cond' <- elaborate cond
    cases' <- forM cases elaborateCase
    return $ Match cond' cases'
    where
        elaborateCase (Case pat e')         = Case pat <$> elaborate e'
        elaborateCase (TypedCase pat ty e') = TypedCase pat ty <$> elaborate e'

elaborate e@(Lambda params body) = Lambda params <$> elaborate body
elaborate e@(FnApp fn args) = FnApp <$> elaborate fn <*> forM args elaborate

elaborate e@(Block stmts) = Block <$> forM stmts elaborateStmt

elaborateStmt stmt@(Return e)      = Return <$> elaborate e
elaborateStmt stmt@(Procedure e)   = Procedure <$> elaborate e
elaborateStmt stmt@(Declaration d) = Declaration <$> elaborateDec d



generateName :: Type -> String
generateName (TVar (Tyvar a _)) = "$tvar_" ++ a
generateName (TLiteral lit)     = "$lit_" ++ generateNameLit lit
generateName (TPrimitive prim)  = "$prim_" ++ show prim
generateName (TTuple tys) = intercalate "_" $ fmap generateName tys
generateName (TRecord pairs) = intercalate "_" $ fmap (\(k, v) -> k ++ "_" ++ generateName v) pairs
generateName (TApplied cons arg) = intercalate "_" ["$app", generateName cons, generateName arg]
generateName (TArrow input output) = intercalate "_" ["$arrow", generateName input, generateName output]
generateName ty = error $ "GenerateName not implemented yet for: " ++ show ty


generateNameLit :: Literal -> String
generateNameLit (LInt i)    = "$int_" ++ show i
generateNameLit (LBool b)   = "$bool_" ++ show b
generateNameLit (LString s) = "$str_" ++ show s




-- typesOf t@(TTuple elems)           = foldl (\set t' -> set <> typesOf t') (Set.singleton t) elems
-- typesOf t@(TRecord pairs)          = foldl (\set (_, t') -> set <> typesOf t') (Set.singleton t) pairs
-- typesOf t@(TUnion tys)             = typesOf (Set.toList tys)
-- typesOf t@(cons `TApplied` arg)    = typesOf cons `Set.union` ftv arg
-- typesOf t@(in' `TArrow` out')          = typesOf t `Set.union` ftv t'
-- typesOf t@(TClosure params body _) = typesOf body
-- typesOf t@(TVar id)                = Set.singleton id
-- typesOf _                        = Set.empty
