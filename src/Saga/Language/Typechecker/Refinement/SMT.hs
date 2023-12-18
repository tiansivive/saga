
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Refinement.SMT where

import           Control.Arrow                               (ArrowChoice (left))
import           Control.Monad.State                         (MonadTrans (lift),
                                                              StateT (runStateT),
                                                              evalStateT, gets)
import           Control.Monad.Trans.State                   (StateT, modify)
import           Data.Map                                    (Map)
import qualified Data.Map                                    as Map

import qualified Data.SBV                                    as SBV
import qualified Data.SBV.Control                            as SBV

import           Effectful                                   (Eff, Effect)
import qualified Effectful                                   as Eff

import           Data.SBV                                    (SBool, SInteger,
                                                              Symbolic, (.&&),
                                                              (.<), (.<=),
                                                              (.==), (.>),
                                                              (.>=), (.||))
import qualified Effectful.Dispatch.Static                   as Eff
import qualified Effectful.Reader.Static                     as Eff
import           Prelude                                     hiding (GT, LT)
import           Saga.Language.Core.Literals                 (Literal (..))
import           Saga.Language.Typechecker.Monad             (TypeCheck)
import qualified Saga.Language.Typechecker.Refinement.Liquid as L
import           Saga.Language.Typechecker.Refinement.Liquid (Liquid (..),
                                                              Op (..), Variable)
import qualified Saga.Language.Typechecker.Type              as T
import           Saga.Language.Typechecker.Type              (Type)



data State = St { nums :: Map (Variable Liquid) SInteger, bools :: Map (Variable Liquid) SBool }
    deriving (Show)
empty :: State
empty = St { nums = mempty, bools = mempty }


type Sym = StateT State Symbolic


translate :: Liquid -> Sym SBool
translate (Var x@(L.Poly v))                   = do
    bs <- gets bools
    case Map.lookup x bs of
        Just x' -> return x'
        Nothing -> do
            x' <- lift $ SBV.free v
            modify $ \s -> s{ bools = Map.insert x x' bs}
            return x'
translate (Boolean b) = return $ SBV.literal b
translate (Negation e)              = SBV.sNot <$> translate e
translate (Logical op left right)   = do
    l <- translate left
    r <- translate right
    return $ case op of
        AND ->  l .&& r
        OR  ->  l .|| r

translate (Comparison op left right) = do
    l <- arithmetic left
    r <- arithmetic right

    return $ case op of
        LT  -> l .< r
        LTE -> l .<= r
        GT  -> l .> r
        GTE -> l .>= r

translate (Equality left right)     = case (left, right) of
    (Number _, _)      -> arithmeticEq
    (l, Number _)      -> arithmeticEq
    (Arithmetic {}, _) -> arithmeticEq
    (_, Arithmetic {}) -> arithmeticEq

    _                  -> (.==) <$> translate left <*> translate right

    where
        arithmeticEq = (.==) <$> arithmetic left <*> arithmetic right

arithmetic :: Liquid -> Sym SInteger
arithmetic (Var x@(L.Poly v))    = do
    ns <- gets nums
    case Map.lookup x ns of
        Just x' -> return x'
        Nothing -> do
            x' <- lift $ SBV.free v
            modify $ \s -> s{ nums = Map.insert x x' ns}
            return x'
arithmetic (Number n) = return . SBV.literal $ toInteger n
arithmetic (Arithmetic op left right) = do
    l <- arithmetic left
    r <- arithmetic right

    return $ case op of
        ADD -> l + r
        SUB -> l - r
        MUL -> l * r
        DIV -> l `SBV.sDiv` r




