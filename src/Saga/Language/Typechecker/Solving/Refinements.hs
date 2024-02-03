{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solving.Refinements where


import           Control.Monad                                 (forM)
import           Control.Monad.State                           (MonadTrans (lift),
                                                                StateT (..),
                                                                evalStateT,
                                                                gets, modify)
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map
import           Data.SBV                                      (SBool, SInteger,
                                                                SatResult (..),
                                                                Symbolic, sNot,
                                                                (.&&), (.<),
                                                                (.<=), (.==),
                                                                (.=>), (.>),
                                                                (.>=), (.||))
import qualified Data.SBV                                      as SBV
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff)
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.State.Static.Local                  as Eff
import           Prelude                                       hiding (GT, LT)
import qualified Saga.Language.Syntax.Elaborated.Types         as T
import qualified Saga.Language.Syntax.Liquids                  as L
import           Saga.Language.Syntax.Liquids
import           Saga.Language.Syntax.Literals                 (Literal (..))
import           Saga.Language.Typechecker.Errors              (SagaError (UnsatisfiableRefinement))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Monad       (Solution (..),
                                                                Solving,
                                                                Status (..))
import           Saga.Language.Typechecker.Substitution        (Substitutable (..))
import           Saga.Utils.Operators                          ((||>))



data State = St { nums :: Map (Variable Liquid) SInteger, bools :: Map (Variable Liquid) SBool }
    deriving (Show)
type Sym = StateT State Symbolic



entails :: Solving es => Solver.Constraint -> [Solver.Constraint] -> Eff es [Solver.Constraint]
entails ref@(Solver.Refinement scope ty liquid) cs = do
    implications <- Eff.liftIO $ forM refinements mkImplication
    let entailments = [ i | i@(SatResult (SBV.Unsatisfiable {})) <- implications ]
    if null entailments then
        return cs
    else return filtered

    where
        mkImplication r = SBV.sat $ do
            (assump, env) <- runStateT (translate r) empty
            proposition <- evalStateT (translate liquid) env
            SBV.constrain assump
            return $ assump .=> sNot proposition


        refinements = [ liq | Solver.Refinement _ _ liq <- filtered ]
        filtered =  [ r | r@(Solver.Refinement scope' ty' liquid') <- cs
                        , liquid /= liquid'
                        ]


solve' :: Solving es => Solver.Constraint -> Eff es (Status, Solver.Constraint)
solve' r@(Solver.Refinement scope ty liquid) = do
    res <- Eff.liftIO . SBV.sat $ evalStateT (translate liquid) empty
    case res of
        SatResult (SBV.Satisfiable _ model) -> return $
            if null $ ftv liquid then
                (Solved, Solver.Empty)
            else (Deferred, Solver.Refinement scope ty liquid)
        _ -> Eff.throwError $ UnsatisfiableRefinement $ Solver.Refinement scope ty liquid


simplify' :: Solving es => Solver.Constraint -> Eff es  Solver.Constraint
simplify' (Solver.Refinement scope it liquid) = do
    proofs' <- Eff.gets proofs
    let subst = scope ||> Map.mapWithKey (convert proofs')
    return $ Solver.Refinement scope it (apply subst liquid)

    where
        convert proofs _ (T.Var tvar) | Just lit <- Map.lookup tvar proofs  = fromLit lit
        convert proofs _ (T.Singleton lit)                                  = fromLit lit
        convert proofs var _                                                = L.Var var

        fromLit (LInt n)  = L.Number n
        fromLit (LBool b) = L.Boolean b






-- | SMT SOLVING

translate :: Liquid -> Sym SBool
translate (Var x@(L.Poly v)) = do
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



empty :: State
empty = St { nums = mempty, bools = mempty }


instance Substitutable Liquid where
    type Target Liquid = Liquid

    apply s l@(L.Var v)           = Map.findWithDefault l v s
    apply s (L.Arithmetic op l r) = L.Arithmetic op (apply s l) (apply s r)
    apply s (L.Comparison op l r) = L.Comparison op (apply s l) (apply s r)
    apply s (L.Logical op l r)    = L.Logical op (apply s l) (apply s r)
    apply s (L.Equality l r)      = L.Equality (apply s l) (apply s r)
    apply s (L.Negation l )       = L.Negation (apply s l)
    apply _ l                     = l

    ftv (L.Var v)            = Set.singleton v
    ftv (L.Arithmetic _ l r) = ftv l <> ftv r
    ftv (L.Comparison _ l r) = ftv l <> ftv r
    ftv (L.Logical _ l r)    = ftv l <> ftv r
    ftv (L.Equality l r)     = ftv l <> ftv r
    ftv (L.Negation l)       = ftv l
    ftv _                    = Set.empty

