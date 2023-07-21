module Saga.AST.TypeSystem.HindleyMilner.Check where
import           Control.Monad.Except                          (Except)
import           Control.Monad.Reader                          (ReaderT)
import           Control.Monad.RWS                             (RWST)
import           Saga.AST.TypeSystem.HindleyMilner.Environment (Infer, TypeEnv)
import           Saga.AST.TypeSystem.HindleyMilner.Inference   (infer)
import           Saga.AST.TypeSystem.HindleyMilner.Types       (Expr, Type)







check :: Expr -> Type -> Infer Bool
check expr ty = do
    ty' <- infer expr
    return $ ty' == ty
