
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Inference.Kind where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Foldable                                 (foldrM)
import qualified Data.Map                                      as Map
import           Saga.Language.Typechecker.Environment
import qualified Saga.Language.Typechecker.Inference.Inference as I
import           Saga.Language.Typechecker.Inference.Inference (Generalize (..),
                                                                InferEff,
                                                                Inference (..),
                                                                Instantiate (..))

import qualified Saga.Language.Typechecker.Kind                as K
import           Saga.Language.Typechecker.Kind

import           Data.Functor                                  ((<&>))
import qualified Data.Set                                      as Set
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Effectful.Writer.Static.Local                 as Eff

import           Effectful                                     (Eff)
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Qualification       (Qualified (..))
import qualified Saga.Language.Typechecker.Solver.Constraints  as CST hiding
                                                                      (Equality)
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint (..))
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import qualified Saga.Language.Typechecker.Type                as T
import           Saga.Language.Typechecker.Type                (Polymorphic,
                                                                Scheme (..),
                                                                Type)
import qualified Saga.Language.Typechecker.TypeExpr            as TE
import           Saga.Language.Typechecker.TypeExpr            (TypeExpr)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables
import           Saga.Utils.Operators                          ((|>))



type KindInference es = InferEff es Counts [UnificationConstraint]
newtype Counts = Counts { kvars:: Int }

type instance I.EmittedConstraint Kind = [UnificationConstraint]
data UnificationConstraint = Empty | Unify Kind Kind

-- | Inferring Kinds of Types
instance Inference TypeExpr where
    type State TypeExpr = Counts
    infer = infer'
    lookup = lookup'
    fresh = fresh'



infer' :: InferEff es Counts [UnificationConstraint] => TypeExpr -> Eff es TypeExpr
infer' te = case te of
    TE.Identifier id -> do
      _ :=> k <- lookup' id
      return $ TE.KindedType te k

    s@(TE.Singleton lit) -> return $ TE.KindedType s K.Type

    TE.Union list ->TE.KindedType <$> (TE.Union <$> mapM infer' list) <*> pure K.Type
    TE.Tuple tup -> TE.KindedType <$> (TE.Tuple <$> mapM infer' tup) <*> pure K.Type
    TE.Record pairs -> TE.KindedType <$> (TE.Record <$> mapM (mapM infer') pairs) <*> pure K.Type

    TE.Arrow in' out' -> do
      kindedIn <- infer' in'
      kindedOut <- infer' out'
      return $ TE.KindedType (kindedIn `TE.Arrow` kindedOut) K.Type

    TE.Implementation p tyExpr -> infer' tyExpr
    TE.Tagged tag tyExpr -> do
        (TE.KindedType ty k) <- infer' tyExpr
        return $ TE.KindedType ty (K.Data tag k)

    TE.Lambda ps@(param : rest) body -> do
        kVar <- fresh'
        let qk = Forall [] (Q.none :=> K.Var kVar)
        let scoped = Eff.local (\e -> e { kinds = Map.insert param qk $ kinds e })
        scoped $ do
            o@(TE.KindedType _ out') <- infer' out
            let k = K.Var kVar `K.Arrow` out'
            let tyExpr = TE.Lambda ps o
            return $ TE.KindedType tyExpr k
        where
            out = case rest of
              [] -> body
              _  -> TE.Lambda rest body


    TE.Application fn [arg] -> do
        out <- K.Var <$> fresh'
        fn'@(TE.KindedType _ fnK)   <- infer' fn
        arg'@(TE.KindedType _ argK) <- infer' arg

        Eff.tell [Unify fnK (argK `K.Arrow` out)]
        return $ TE.KindedType (TE.Application fn' [arg']) out
    TE.Application fn (a : as) -> infer' curried
        where
          partial = TE.Application fn [a]
          curried = foldl (\f a -> TE.Application f [a]) partial as

    te' -> error $ "Kind Inference not yet implemented for: " ++ show te'



lookup' :: KindInference es => String -> Eff es (Qualified Kind)
lookup' x = do
  Saga { kinds } <- Eff.ask
  case Map.lookup x kinds of
    Just scheme -> walk scheme
    Nothing     -> Eff.throwError $ UnboundVariable x

  where
    walk scheme@(Forall [] qk)   = return qk
    walk scheme@(Forall tvars _) = fresh' >>= walk . instantiate scheme . K.Var



fresh' :: KindInference es =>  Eff es (Variable Kind)
fresh' = do
  i <- Eff.gets $ kvars |> (+1)
  Eff.modify $ \s -> s { kvars = i }
  let count = show ([1 ..] !! i)
  return $ K.Poly ("t" ++ count) K.Kind


initialState :: Counts
initialState = Counts 0

run :: Eff (Eff.Writer [UnificationConstraint] : Eff.State Counts : es) a -> Eff es (a, [UnificationConstraint])
run  = Eff.evalState initialState . Eff.runWriter

instance Instantiate Kind  where
  instantiate qt@(Forall [] _) t = qt
  instantiate (Forall (tvar:tvars) qt) t = Forall tvars qt'
    where
      sub = Map.fromList [(tvar, t)]
      qt' = apply sub qt

instance Generalize Kind where
    type St Kind = ()
    generalize k = return $ Forall (Set.toList $ ftv k) (Q.none :=> k)

class HasKind t where
  kind :: KindInference es => t -> Eff es Kind

instance HasKind Type where
  kind (T.Data _ k) = return k
  kind (T.Var v)    = kind v
  kind (T.Applied f _) = do
    k <- kind f
    case k of
      (K.Arrow _ k') -> return k'
      k'             -> Eff.throwError $ UnexpectedKind k' "Tried to apply a type to a non Arrow Kind"

  kind (T.Closure ps tyExpr closure) = do
    TE.KindedType _ k <- infer' tyExpr
    foldrM mkArrow k ps
    where
        mkArrow v k = do
            k' <- kind v
            return $ K.Arrow k' k

  kind  _ = return K.Type


instance HasKind (Variable Type) where
    kind (T.Poly _ k)        = return k
    kind (T.Existential _ k) = return k
    kind (T.Local _ k)       = return k


