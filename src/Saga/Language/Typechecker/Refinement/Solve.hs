{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Refinement.Solve where
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import qualified Effectful                                    as Eff
import qualified Effectful.Error.Static                       as Eff
import qualified Effectful.Reader.Static                      as Eff
import           Prelude                                      hiding (EQ, GT,
                                                               LT, lookup)
import           Saga.Language.Core.Literals                  (Literal (..))
import           Saga.Language.Typechecker.Environment        (CompilerState,
                                                               Info)
import           Saga.Language.Typechecker.Errors             (Exception (..),
                                                               SagaError (..),
                                                               crash)
import qualified Saga.Language.Typechecker.Monad              as TC
import           Saga.Language.Typechecker.Monad              (TypeCheck)
import qualified Saga.Language.Typechecker.Qualification      as Q
import qualified Saga.Language.Typechecker.Refinement.Liquid  as L

import qualified Effectful.Writer.Static.Local                as Eff
import qualified Saga.Language.Typechecker.Refinement.Liquid  as Op
import           Saga.Language.Typechecker.Refinement.Liquid  (Liquid (..),
                                                               Op (..))
import qualified Saga.Language.Typechecker.Solver.Constraints as CST
import           Saga.Language.Typechecker.Solver.Constraints (Assumption)
import qualified Saga.Language.Typechecker.Type               as T
import           Saga.Language.Typechecker.Type               (Type)
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)



data Result
    = Solved Literal
    | Constraint Liquid
    deriving Show

type Refined = TypeCheck '[Eff.Reader Environment, Eff.Writer [Constraint]]

type Environment = Map String Type

type Constraint = (String, Liquid)



run :: Refined a -> Either String (Either (Eff.CallStack, SagaError) ((a, [Constraint]), Info))
run (a:: Refined a) = Eff.runPureEff $ TC.run $ Eff.runReader @Environment Map.empty $ Eff.runWriter @[Constraint] $ Eff.inject a




-- solve (Var v) = lookup v
-- solve (Negation expr) = solve expr >>= \case
--     neg@(Number l)          -> Eff.throwError . UnexpectedLiquidNegation $ neg
--     neg@(Arithmetic {})     -> Eff.throwError . UnexpectedLiquidNegation $ neg

--     Boolean bool            -> return . Boolean $ not bool
--     e                       -> return . Negation $ e

-- solve (Arithmetic op left right) = do
--     where
--         reduce ADD (Number left) (Number right)    = Number $ l + r
--         reduce SUB (Number left) (Number right)    = Number $ l - r
--         reduce MUL (Number left) (Number right)    = Number $ l * r
--         reduce DIV (Number left) (Number right)
--             | right == 0    = crash $ DivideByZero left right
--             | otherwise     = Number $ l / r

-- solve (BinaryOp Op.AND left right) = do
--     (Literal (LBool l), leftCs)  <- Eff.listen @[Constraint] $ solve left
--     (Literal (LBool r), rightCs) <- Eff.listen @[Constraint] $ solve right

--     solve' op left' right'

--     where
--         solve' op (Var x) right = do
--             Eff.tell [(x, BinaryOp op (Var x) right)]

-- lookup :: String -> Refined Liquid
-- lookup id = do
--     env <- Eff.ask @Environment
--     return $ case Map.lookup id env of
--         Just (T.Singleton lit) -> Literal lit
--         _                      -> Var id

-- -- solve neg@(Negation expr) = solve expr >>= \case
-- --     Solved (LBool bool) -> return . Solved . LBool $ not bool
-- --     Constraint l        -> return . Constraint . Negation $ l
-- --     Solved lit          -> Eff.throwError . UnexpectedLiquidNegation $ neg

-- solve (BinaryOp op left right) = do
--     left'  <- solve left
--     right' <- solve right

--     solve' op left' right'

--     where
--         solve' :: Op -> Result -> Result -> Refined Result
--         solve' op left right
--             | Constraint l <- left, Constraint r  <- right = return . Constraint $ BinaryOp op l r
--             | Constraint e <- left, Solved right' <- right = return . Constraint $ BinaryOp op e (Literal right')
--             | Solved left' <- left, Constraint e  <- right = return . Constraint $ BinaryOp op (Literal left') e
--             | Solved left' <- left, Solved right' <- right = Eff.throwError $ UnexpectedUnsimplifiedExpr op (Literal left') (Literal right')

-- lookup :: String -> Refined Result
-- lookup id = do
--     env <- Eff.ask @Environment
--     return $ case Map.lookup id env of
--         Just (T.Singleton lit) -> Solved lit
--         _                      -> Constraint $ Var id

-- simplify :: Liquid -> Liquid
-- simplify e@(BinaryOp {}) = if combined == e' then e' else simplify combined

--     where
--         e'@(BinaryOp op left right) = normalize e
--         left' = simplify left
--         right' = simplify right

--         BinaryOp _ l r = distribute . associate . commute $ BinaryOp op left' right'
--         combined = reduce op (simplify l) (simplify r)


-- simplify (Negation expr) | Literal (LBool b) <- expr    = Literal . LBool $ not b
--                            | otherwise                      = Negation $ simplify expr
-- simplify expr = expr

-- normalize :: Liquid -> Liquid
-- normalize (BinaryOp op left right) = normalize' op (normalize left) (normalize right)
--     where
--         normalize' :: Op -> Liquid -> Liquid -> Liquid
--         normalize' GT  left right = BinaryOp LT  right left
--         normalize' GTE left right = BinaryOp LTE right left
--         normalize' LT  left right = BinaryOp LT  left right
--         normalize' LTE left right = BinaryOp LTE left right

--         -- Add normalization for other binary operators if necessary
--         normalize' op left right  = BinaryOp op left right  -- Default cas

-- normalize expr = expr  -- Non-binary operations remain unchanged


-- -- | Term reduction
-- reduce :: Op -> Liquid -> Liquid -> Liquid
-- reduce AND (Literal left) (Literal right) | LBool l <- left, LBool r <- right            = Literal . LBool $ l && r

-- reduce AND (BinaryOp LT v1 (Literal lit1)) (BinaryOp LT v2 (Literal lit2)) | v1 == v2 = BinaryOp LT v1 (Literal $ min lit1 lit2)

-- reduce OR  (Literal left) (Literal right) | LBool l <- left, LBool r <- right            = Literal . LBool $ l || r

-- reduce ADD (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LInt $ l + r
-- reduce SUB (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LInt $ l - r
-- reduce MUL (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LInt $ l * r
-- reduce DIV (Literal left) (Literal right)
--     | LInt r <- right, r == 0                                                                   = crash $ DivideByZero left right
--     | LInt l <- left, LInt r <- right, r /= 0                                                   = Literal . LInt $ l / r

-- reduce GT  (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LBool $ l > r
-- reduce GTE (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LBool $ l >= r
-- reduce LT  (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LBool $ l < r
-- reduce LTE (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LBool $ l <= r
-- reduce EQ  (Literal left) (Literal right) | LInt l <- left, LInt r <- right              = Literal . LBool $ l == r

-- reduce op left right = BinaryOp op left right


-- -- | Commutative law
-- commute :: Liquid -> Liquid
-- commute (BinaryOp op a b)
--     | commutative op && shouldCommute a b = BinaryOp op b a
--     | otherwise                           = BinaryOp op a b
--     where
--         shouldCommute :: Liquid -> Liquid -> Bool
--         shouldCommute left (Literal _) | not $ isLiteral left = True  -- Prefer literals first
--         shouldCommute _ _                                       = False
-- commute expr = expr

-- commutative :: Op -> Bool
-- commutative ADD = True
-- commutative MUL = True
-- commutative AND = True
-- commutative OR  = True
-- commutative _   = False


-- -- | Associativity law
-- associate :: Liquid -> Liquid
-- associate (BinaryOp op (BinaryOp op' a b) c)
--     | op == op' && associative op && (isLiteral b || isLiteral c) =
--         BinaryOp op a (BinaryOp op b c)
-- associate (BinaryOp op a (BinaryOp op' b c))
--     | op == op' && associative op && (isLiteral a || isLiteral b) =
--         BinaryOp op (BinaryOp op a b) c
-- associate expr = expr

-- associative :: Op -> Bool
-- associative ADD = True
-- associative MUL = True
-- associative AND = True
-- associative OR  = True
-- associative _   = False


-- -- | Distributive Law
-- distribute :: Liquid -> Liquid
-- distribute (BinaryOp MUL a (BinaryOp ADD b c)) = BinaryOp ADD (BinaryOp MUL a b) (BinaryOp MUL a c)
-- distribute (BinaryOp MUL (BinaryOp ADD a b) c) = BinaryOp ADD (BinaryOp MUL a c) (BinaryOp MUL b c)
-- distribute expr = expr



-- isLiteral :: Liquid -> Bool
-- isLiteral (Literal _) = True
-- isLiteral _           = False

