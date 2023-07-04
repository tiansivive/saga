module Saga.AST.TypeSystem.HindleyMilner.Refinement where
import           Control.Monad.Except                          (ExceptT,
                                                                MonadError (throwError))
import           Control.Monad.State.Lazy
import qualified Data.Map                                      as Map
import           Debug.Trace                                   (trace, traceM)
import qualified Saga.AST.TypeSystem.HindleyMilner.Environment as E
import           Saga.AST.TypeSystem.HindleyMilner.Environment
import           Saga.AST.TypeSystem.HindleyMilner.Types



refine :: TypeExpr -> Infer Type
refine a | trace ("refine: " ++ show a) False = undefined
refine (Type ty)                      = return ty
refine (TParens tyExp)                = refine tyExp
refine (TClause _ tyExp)              = refine tyExp
refine (TBlock [])                    = return TUnit
refine (TBlock tyExps)                = refine $ last tyExps
refine (TReturn tyExp)                = refine tyExp
refine (TConditional cond true false) = refine true -- assumes same return type, will change with union types
refine (TIdentifier id)               = do
    tyExpr <- lookupAlias id
    refine tyExpr

refine (TFnApp fnExpr argExprs)    = do
    args <- mapM refine argExprs
    constructor <- refine fnExpr

    traceM $ "Fn Expression: " <> show constructor
    traceM $ "Fn Args: " <> show args


    case (constructor, argExprs) of
        (TVar t, []) -> do
            tyExpr <- lookupAlias t
            refine tyExpr
        -- (TVar t, [last]) -> return $ TParametric t last
        -- (TVar id, tyExpr:tail) -> do
        --     out <- evalScoped $ refine $ TFnApp info tyExpr tail
        --     return $ TParametric t (Type out)

        (TParametric _ body, []) -> refine body
        (TParametric [param] body, [tyExpr]) -> do
            bind param tyExpr
            refine body
        (TParametric (p:ps) body, tyExpr:es) -> do
            bind p tyExpr
            let lambda = TLambda ps body
            refine $ TFnApp lambda es

        _ -> throwError $ UnexpectedType "Cannot apply this type expression"

    where
      bind arg tyExpr = do
        (Env _ aliases _) <- E.get
        E.modify $ \s -> s{ typeAliases = Map.insert arg tyExpr aliases }

refine (TLambda params body) = return $ TParametric params body



lookupAlias :: Alias -> Infer TypeExpr
lookupAlias a = do
    (Env _ alias _) <- E.get
    case Map.lookup a alias of
        Nothing -> throwError $ UndefinedIdentifier a
        Just t  -> return t



-- runScoped :: Infer a -> Infer (a, TypeEnv)
-- runScoped action = do
--     env <- E.get
--     lift $ runStateT action env

-- evalScoped :: (MonadState s (t m), MonadTrans t, Monad m) => StateT s m b -> t m b
-- evalScoped action = do
--     env <- E.get
--     lift $ evalStateT action env
