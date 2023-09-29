module Saga.Language.TypeSystem.Elaboration where

import           Control.Monad.Except
import           Control.Monad.Reader                 (Reader, runReader)
import           Control.Monad.RWS
import           Control.Monad.State                  (State, get, runState)
import           Data.Functor                         ((<&>))
import           Data.List                            (intercalate, intersect)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Prelude                              hiding (log)
import           Saga.Language.Core.Literals          (Literal (..))
import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.Constraints (ftv)
import           Saga.Language.TypeSystem.Environment (CompilerState, Protocol,
                                                       Saga, Tell (Error),
                                                       UnificationVar, log)
import           Saga.Language.TypeSystem.Errors      (SagaError)
import           Saga.Language.TypeSystem.Inference   hiding (log)
import           Saga.Language.TypeSystem.Types
import           Saga.Utils.Utils                     ((|>), (||>))






-- elaborate :: Declaration -> Saga (Except SagaError) Declaration
-- elaborate dec = case dec of
--     Let id (Just tyExpr) k expr -> do
--         (expr', _) <- return $ runReader (transform expr tyExpr) Map.empty
--         return $ Let id (Just tyExpr) k expr'



-- elaborate :: MonadReader CompilerState m => Expr -> m (Either SagaError TypeExpr)


-- elaborate e@(Literal lit) = do
--     env <- ask
--     let ty = runExcept $ runInfer env $ infer e
--     return (e, ty)

-- elaborate e@(List l) = do
--     foo <- mapM elaborate l
--     env <- ask
--     let ty = runExcept $ runInfer env $ infer e
--     return (e, ty)



--newtype Typed a = Typed (a, Either SagaError TypeExpr)


-- type ConstraintEnv = Map.Map Tyvar Protocol

-- elaborate :: MonadReader ConstraintEnv m => Expr -> m b
-- elaborate (Lambda params body) = Lambda params <$> elaborate body
-- elaborate (FnApp fn args) = do
--     dicts <- getProtocolRecords fn

--     args' <- mapM elaborate args
--     return $ FnApp (FnApp fn dicts) args'

-- getProtocolRecords :: Monad m => Expr -> m [Expr]
-- getProtocolRecords (Identifier id) = do
--     ty <- _lookup id
--     case ty of
--         TQualified (cs :=> tyExpr) -> do
--             _impls

--     -- vars <- asks Map.keys
--     -- let relevant = vars `intersect` tvars
--     -- Map.
    -- where
    --     tvars = Set.toList $ ftv ty




type Transform = Reader Dictionaries
type Dictionaries = Map Type String

transform :: Expr -> Transform Expr




transform (Lambda params body) = do
    --modify $ Map.union (Map.fromList dicts)
    body' <- transform body
    -- | TODO: this seems incorrect? depends on the lambda, it might not need it?
    -- | I guess we have to first check the expr to get all the constraints, bottom up, and then lift it all up to the top?
    dicts <- asks Map.toList
    return $ Lambda (snd <$> dicts) (Lambda params body')
    --where
       -- dicts = zipWith (\(ty `Implements` prtcl) i -> (ty, intercalate "_" ["dict", prtcl, generateName ty])) cs [0..]





generateName :: Type -> String
generateName (TVar (Tyvar a _)) = "tvar_" ++ a
generateName (TLiteral lit)     = "lit_" ++ generateNameLit lit
generateName (TPrimitive prim)  = "prim_" ++ show prim
generateName (TTuple tys) = intercalate "_" $ fmap generateName tys
generateName (TRecord pairs) = intercalate "_" $ fmap (\(k, v) -> k ++ "_" ++ generateName v) pairs
generateName (TApplied cons arg) = intercalate "_" ["app", generateName cons, generateName arg]
generateName (TArrow input output) = intercalate "_" ["arrow", generateName input, generateName output]
generateName ty = error $ "GenerateName not implemented yet for: " ++ show ty


generateNameLit :: Literal -> String
generateNameLit (LInt i)    = "int_" ++ show i
generateNameLit (LBool b)   = "bool_" ++ show b
generateNameLit (LString s) = "str_" ++ show s
