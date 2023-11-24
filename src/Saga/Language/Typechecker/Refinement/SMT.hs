{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}
module Saga.Language.Typechecker.Refinement.SMT where

import           Control.Arrow                               (ArrowChoice (left))
import           Control.Monad.State                         (MonadTrans (lift),
                                                              StateT (runStateT),
                                                              evalState,
                                                              evalStateT, gets)
import           Control.Monad.Trans.State                   (StateT, modify)
import           Data.Map                                    (Map)
import qualified Data.Map                                    as Map
import           Data.SBV
import qualified Data.SBV                                    as SBV
import qualified Data.SBV.Control                            as SBV
import           Data.SBV.Internals                          (SMTModel (..))
import qualified Effectful.Reader.Static                     as Eff
import           Prelude                                     hiding (GT, LT)
import           Saga.Language.Core.Literals                 (Literal (..))
import           Saga.Language.Typechecker.Monad             (TypeCheck)
import           Saga.Language.Typechecker.Refinement.Liquid (Liquid (..),
                                                              Op (..))
import qualified Saga.Language.Typechecker.Type              as T
import           Saga.Language.Typechecker.Type              (Type)
import           Text.Pretty.Simple                          (pPrint)

--type Refined = TypeCheck '[Eff.Reader Environment]
data State = St { nums :: Map String SInteger, bools :: Map String SBool }
empty :: State
empty = St { nums = mempty, bools = mempty }
-- data Sym s :: Effect
-- type instance Eff.DispatchOf (Sym s) = Eff.Static Eff.WithSideEffects
-- newtype instance Eff.StaticRep (Sym s)  = Sym s
-- -- data SymbolicEffect :: Effect where
--   RunSymbolic :: Symbolic a -> SymbolicEffect m a

type Sym = StateT State Symbolic


translate :: Liquid -> Sym SBool
translate (Var x)                   = do
    bs <- gets bools
    case Map.lookup x bs of
        Just x' -> return x'
        Nothing -> do
            x' <- lift $ SBV.free x
            modify $ \s -> s{ bools = Map.insert x x' bs}
            return x'
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
    ns <- gets nums
    case Map.lookup x ns of
        Just x' -> return x'
        Nothing -> do
            x' <- lift $ SBV.free x
            modify $ \s -> s{ nums = Map.insert x x' ns}
            return x'
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
run expr = evalStateT (translate expr) empty

test :: Liquid -> IO ()
test expr = do
    runSMT (run expr) >>= pPrint
    result <- sat (run expr)
    case result of
        SatResult (SBV.Satisfiable _ model) -> do
            pPrint model
            putStrLn "Satisfiable with constraints:"
            mapM_ pPrint (modelAssocs model)
        res -> pPrint res




-- translate ::  Liquid -> Refined (Symbolic SBool)
-- translate expr = case expr of
--     Number n -> return  $ shiftL n
--     Literal lit -> return $ translate' lit
--     Var x       -> lookup x
--     where
-- translate ::  Liquid -> Refined (SBV a)
-- translate expr = case expr of
--     Literal lit -> return $ translate' lit
--     Var x       -> lookup x
--     where

--         lookup :: String -> Refined (SBV Bool)
--         lookup id = do
--             env <- Eff.ask @Environment
--             return $ case Map.lookup id env of
--                 Just (T.Singleton lit) -> translate' lit
--                 _                      -> _free id

--         translate' (LInt x)  = literal x
--         translate' (LBool b) = literal b

-- translateLiteral :: Literal -> SBV a
-- translateLiteral (LInt i)    = literal i
-- translateLiteral (LBool b)   = literal b
-- -- Handling of strings needs special attention
-- translateLiteral (LString s) = error "String"
-- -- translate
-- -- translate (BinaryOp op a b) = translateOp op (translate env a) (translate env b)
-- -- translate (Negation a)      = bnot (translate env a)




