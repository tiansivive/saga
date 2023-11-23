{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}
module Saga.Language.Typechecker.Refinement.SMT where

import           Control.Arrow                                    (ArrowChoice (left))
import           Control.Monad.State                              (MonadTrans (lift),
                                                                   StateT (runStateT),
                                                                   evalState,
                                                                   evalStateT,
                                                                   gets)
import           Control.Monad.Trans.Reader                       (ReaderT (runReaderT),
                                                                   asks)
import           Control.Monad.Trans.State                        (StateT,
                                                                   modify)
import           Control.Monad.Trans.Writer                       (WriterT)
import qualified Control.Monad.Trans.Writer                       as W
import           Data.Map                                         (Map)
import qualified Data.Map                                         as Map
import           Data.SBV
import qualified Data.SBV                                         as SBV
import qualified Data.Set                                         as Set
import           Debug.Pretty.Simple                              (pTraceM)
import           Documentation.SBV.Examples.ProofTools.Strengthen (S (x))
import qualified Effectful.Reader.Static                          as Eff
import           Prelude                                          hiding (GT,
                                                                   LT)
import           Saga.Language.Core.Literals                      (Literal (..))
import           Saga.Language.Typechecker.Errors                 (Exception (..),
                                                                   crash)
import           Saga.Language.Typechecker.Monad                  (TypeCheck)
import           Saga.Language.Typechecker.Refinement.Liquid      (Liquid (..),
                                                                   Op (..))
import qualified Saga.Language.Typechecker.Type                   as T
import           Saga.Language.Typechecker.Type                   (Type)
import           Text.Pretty.Simple                               (pPrint)

--type Refined = TypeCheck '[Eff.Reader Environment]
data Vars = Vars { nums :: Map String SInteger, bools :: Map String SBool }
empty :: Vars
empty = Vars { nums = mempty, bools = mempty }
-- data Sym s :: Effect
-- type instance Eff.DispatchOf (Sym s) = Eff.Static Eff.WithSideEffects
-- newtype instance Eff.StaticRep (Sym s)  = Sym s
-- -- data SymbolicEffect :: Effect where
--   RunSymbolic :: Symbolic a -> SymbolicEffect m a

type Sym = ReaderT Vars Symbolic





translate :: Liquid -> Sym SBool
translate (Var x)                   = do
    bs <- asks bools
    case Map.lookup x bs of
        Just x' -> return x'
        Nothing -> crash $ Unexpected "Refinement Variable" x

translate (Boolean b) = return $ literal b
translate (Negation e)              = sNot <$> translate e
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
arithmetic (Var x)    = do
    ns <- asks nums
    case Map.lookup x ns of
        Just x' -> return x'
        Nothing -> crash $ Unexpected "Refinement Variable" x

arithmetic (Number n) = return . literal $ toInteger n
arithmetic (Arithmetic op left right) = do
    l <- arithmetic left
    r <- arithmetic right

    return $ case op of
        ADD -> l + r
        SUB -> l - r
        MUL -> l * r
        DIV -> l `sDiv` r

run :: Liquid -> Symbolic SBool
run expr = do
    let vs = Set.toList $ fvs expr
    vars <- mapM (SBV.free @Integer) vs
    runReaderT (translate expr) $ Vars { bools = mempty, nums = Map.fromList $ zip vs vars}

test :: Liquid -> IO ()
test expr = do
    runSMT (run expr) >>= pPrint
    sat (run expr) >>= pPrint




fvs :: Liquid -> Set.Set String
fvs (Var x)                   = Set.singleton x
fvs (Arithmetic _ left right) = fvs left <> fvs right
fvs (Comparison _ left right) = fvs left <> fvs right
fvs (Logical _ left right)    = fvs left <> fvs right
fvs (Equality left right)     = fvs left <> fvs right
fvs (Negation e)              = fvs e
fvs _                         = Set.empty
