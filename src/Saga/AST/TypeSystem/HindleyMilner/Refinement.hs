module Saga.AST.TypeSystem.HindleyMilner.Refinement where
import           Control.Monad.Except                          (MonadError (throwError))
import           Control.Monad.State.Lazy
import qualified Data.Map                                      as Map
import           Debug.Trace                                   (trace, traceM)
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import           Saga.AST.TypeSystem.HindleyMilner.Types



refine :: TypeExpr -> Infer Type
refine a | trace ("refine: " ++ show a) False = undefined
refine (Type _ ty)                      = return ty
refine (TParens _ tyExp)                = refine tyExp
refine (TClause _ _ tyExp)              = refine tyExp
refine (TBlock _ [])                    = return TUnit
refine (TBlock _ tyExps)                = refine $ last tyExps
refine (TReturn _ tyExp)                = refine tyExp
refine (TConditional _ cond true false) = refine true -- assumes same return type, will change with union types
refine (TIdentifier _ id)               = do
    tyExpr <- lookupAlias id
    refine tyExpr

refine (TFnApp info fnExpr argExprs)    = do
    args <- mapM refine argExprs
    constructor <- refine fnExpr

    traceM $ "Fn Expression: " <> show constructor
    traceM $ "Fn Args: " <> show args


    case (constructor, argExprs) of
        (TVar t, []) -> do
            tyExpr <- evalScoped $ lookupAlias t
            refine tyExpr
        -- (TVar t, [last]) -> return $ TParametric t last
        -- (TVar id, tyExpr:tail) -> do
        --     out <- evalScoped $ refine $ TFnApp info tyExpr tail
        --     return $ TParametric t (Type out)

        (TParametric _ body, []) -> refine body
        (TParametric [param] body, [tyExpr]) -> do
            (_, env') <- runScoped $ bind param tyExpr
            lift $ evalStateT (refine body) env'
        (TParametric (p:ps) body, tyExpr:es) -> do
            (_, env') <- runScoped $ bind p tyExpr
            let lambda = TLambda info ps body
            lift $ evalStateT (refine $ TFnApp info lambda es) env'

        _ -> throwError $ UnexpectedType "Cannot apply this type expression"

    where
      bind arg tyExpr = do
        (Env _ aliases _) <- get
        modify $ \s -> s{ typeAliases = Map.insert arg tyExpr aliases }

refine (TLambda info params body) = return $ TParametric params body



lookupAlias :: Alias -> Infer TypeExpr
lookupAlias a = do
    (Env _ alias _) <- get
    case Map.lookup a alias of
        Nothing -> throwError $ UndefinedIdentifier a
        Just t  -> return t


runScoped :: (MonadState s (t m), MonadTrans t, Monad m) => StateT s m a -> t m (a, s)
runScoped action = do
    env <- get
    lift $ runStateT action env

evalScoped :: (MonadState s (t m), MonadTrans t, Monad m) => StateT s m b -> t m b
evalScoped action = do
    env <- get
    lift $ evalStateT action env
