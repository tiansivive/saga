module Saga.Language.TypeSystem.Elaboration where

import           Control.Monad.Except
import           Control.Monad.Reader                 (Reader, runReader)
import           Control.Monad.RWS
import           Control.Monad.State                  (State, get, runState)
import           Data.Functor                         ((<&>))
import           Data.List                            (find, intercalate,
                                                       intersect)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe
import qualified Data.Set                             as Set
import           Data.Traversable                     (for)
import           Debug.Trace                          (traceM)
import           Prelude                              hiding (log)
import           Saga.Language.Core.Literals          (Literal (..))
import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.Constraints (ftv)
import           Saga.Language.TypeSystem.Environment (CompilerState (types, values),
                                                       Protocol (Protocol),
                                                       ProtocolID, Saga,
                                                       Tell (Error),
                                                       UnificationVar, log)
import           Saga.Language.TypeSystem.Errors      (SagaError (..))
import           Saga.Language.TypeSystem.Inference   hiding (log)
import           Saga.Language.TypeSystem.Types
import           Saga.Utils.Utils                     ((|>), (||>))






-- elaborate :: Declaration -> [Constraint] -> Saga (Except SagaError) Declaration
-- elaborate dec cs = case dec of
--     Let id (Just tyExpr) k expr -> do
--         (expr', _) <- return $ runReader (transform expr tyExpr) Map.empty
--         return $ Let id (Just tyExpr) k expr'

--     where
--         dictionaries = fmap dic cs
--         dic (Implements ty p) = (ty, p, generateName ty ++ "_$" ++ p)


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


elaborateDec :: Declaration -> Elaboration Declaration
elaborateDec (Let id ty k e) = Let id ty k <$> case ty of
    Just ty' -> elaborate' ty' e
    Nothing  -> elaborate e

    where
        elaborate' (TQualified (cs :=> ty)) e = do
            dicts <- ask
            Lambda (Map.toList dicts ||> fmap snd) <$> elaborate e
        elaborate' t e = elaborate e
elaborateDec d = return d



elaborate :: Expr -> Elaboration Expr
elaborate e@(Typed (Identifier id) ty') = do
    --st <- get

    ty <- gets $ values |> Map.lookup id
    dictMap <- ask
    traceM "elaborating ID in fn app:"
    traceM $ "\tLooked up: " ++ show ty
    traceM $ "\tDict Map: " ++ show dictMap
    --traceM $ "\n\tState: " ++ show st
    case ty of
        Just (TQualified (cs :=> tyExpr)) -> do
            let dicts = cs ||> combos |> mapMaybe (`Map.lookup` dictMap)
            case dicts of
                [d] -> return $ FnApp (Identifier id) [Identifier d]
                []  -> throwError $ Fail $ "Could not find any suitable protocols for " ++ show id
                ds  -> throwError $ Fail $ "Ambiguous protocols for:\n\tId: " ++ show id ++ "\n\tProtocols: " ++ show ds
        _ -> return e
    where
        tvars = Set.toList $ ftv ty'
        protocols = fmap (\(Implements t p) -> p)
        combos cs = do
            tvar <- tvars
            p <- protocols cs
            return (tvar, p)

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
