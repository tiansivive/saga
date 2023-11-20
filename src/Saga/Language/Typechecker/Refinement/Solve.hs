{-# LANGUAGE DataKinds #-}
module Saga.Language.Typechecker.Refinement.Solve where
import qualified Effectful.Error.Static                       as Eff
import qualified Effectful.Reader.Static                      as Eff
import           Saga.Language.Core.Literals                  (Literal (..))
import           Saga.Language.Typechecker.Environment        (CompilerState)
import           Saga.Language.Typechecker.Errors             (SagaError (UnexpectedLiquidNegation))
import           Saga.Language.Typechecker.Monad              (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification      as Q
import qualified Saga.Language.Typechecker.Refinement.Liquid  as L
import           Saga.Language.Typechecker.Refinement.Liquid  (Liquid)
import qualified Saga.Language.Typechecker.Solver.Constraints as CST
import           Saga.Language.Typechecker.Solver.Constraints (Assumption)
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)



data Result
    = Solved Literal
    | Constraint Liquid

type Refined = TypeCheck '[Eff.Reader [Assumption]]



lookup :: String -> Refined Liquid
lookup id = do
    assumps <- Eff.ask @[Assumption]
    let as' = [ (ty, liquid) | CST.ValueLevel c@(Q.Refinement liquid ty) <- assumps]

    _r


solve :: Liquid -> Refined Result
solve (L.Literal lit) = return $ Solved lit

solve v@(L.Identifier var) = return $ Constraint v

solve neg@(L.Negation (L.Identifier {})) = return $ Constraint neg
solve neg@(L.Negation expr) = do
    result <- solve expr
    case result of
        Solved (LBool bool) -> return . Solved . LBool $ not bool
        Constraint l        -> return . Constraint . L.Negation $ l
        Solved lit          -> Eff.throwError . UnexpectedLiquidNegation $ neg

