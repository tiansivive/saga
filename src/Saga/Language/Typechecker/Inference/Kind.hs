{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Inference.Kind where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Foldable                                 (foldrM)
import qualified Data.Map                                      as Map
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Inference.Inference as I
import           Saga.Language.Typechecker.Inference.Inference (Generalize (..),
                                                                InferM,
                                                                Inference (..),
                                                                Instantiate (..),
                                                                State (..),
                                                                Tag (..), emit')

import qualified Saga.Language.Typechecker.Kind                as K
import           Saga.Language.Typechecker.Kind

import           Data.Functor                                  ((<&>))
import qualified Data.Set                                      as Set
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.State.Static.Local                  as Eff
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

type instance VarType TypeExpr I.Evidence       = Var.PolymorphicVar CST.Evidence
type instance VarType TypeExpr I.Unification    = Var.PolymorphicVar Kind
type instance VarType TypeExpr I.Skolem         = Var.PolymorphicVar Kind
type instance VarType TypeExpr I.TypeVar        = Var.PolymorphicVar Kind
type instance VarType TypeExpr I.Instantiation  = Var.PolymorphicVar Kind


type instance I.EmittedConstraint Kind = UnificationConstraint

data UnificationConstraint = Empty | Unify Kind Kind
type KindInference = InferM UnificationConstraint

-- | Inferring Kinds of Types
instance Inference TypeExpr where

    infer = infer'
    lookup = lookup'
    fresh = fresh'



infer' :: TypeExpr -> KindInference TypeExpr
infer' te = case te of
    TE.Identifier id -> do
      _ :=> k <- lookup' id
      return $ TE.KindedType te k

    s@(TE.Singleton lit) -> return $ TE.KindedType s K.Type

    TE.Union list -> TE.KindedType <$> (TE.Union <$> mapM infer' list) <*> pure K.Type
    TE.Tuple tup -> TE.KindedType <$> (TE.Tuple <$> mapM infer' tup) <*> pure K.Type
    TE.Record pairs -> TE.KindedType <$> (TE.Record <$> mapM (mapM infer') pairs) <*> pure K.Type

    TE.Arrow in' out' -> do
      kindedIn <- infer' in'
      kindedOut <- infer' out'
      return $ TE.KindedType (kindedIn `TE.Arrow` kindedOut) K.Type

    TE.Implementation p tyExpr -> infer tyExpr
    TE.Tagged tag tyExpr -> do
        (TE.KindedType ty k) <- infer tyExpr
        return $ TE.KindedType ty (K.Data tag k)

    TE.Lambda ps@(param : rest) body -> do
        kVar <- fresh' U
        let qk = Forall [] (pure $ K.Var kVar)
        let scoped = Eff.local (\e -> e { kinds = Map.insert param qk $ kinds e })
        scoped $ do
            o@(TE.KindedType _ out') <- infer out
            let k = K.Var kVar `K.Arrow` out'
            let tyExpr = TE.Lambda ps o
            return $ TE.KindedType tyExpr k
        where
            out = case rest of
              [] -> body
              _  -> TE.Lambda rest body


    TE.Application fn [arg] -> do
        out <- K.Var <$> fresh' U
        fn'@(TE.KindedType _ fnK)   <- infer' fn
        arg'@(TE.KindedType _ argK) <- infer' arg

        inferred <- generalize $ argK `K.Arrow` out
        emit' $ Unify fnK out
        return $ TE.KindedType (TE.Application fn' [arg']) out
    TE.Application fn (a : as) -> infer curried
        where
          partial = TE.Application fn [a]
          curried = foldl (\f a -> TE.Application f [a]) partial as

    te' -> error $ "Kind Inference not yet implemented for: " ++ show te'




lookup' :: String -> KindInference (Qualified Kind)
lookup' x = do
  Saga { kinds } <- Eff.ask
  case Map.lookup x kinds of
    Just scheme -> walk scheme
    Nothing     -> Eff.throwError $ UnboundVariable x

  where
    walk scheme@(Forall [] qk)   = return qk
    walk scheme@(Forall tvars _) = fresh' U >>= walk . instantiate scheme . K.Var



fresh' :: Tag a -> InferM w (VarType TypeExpr a)
fresh' t = do
  Eff.modify $ \s -> s {vars = vars s + 1}
  s <- Eff.get
  let count = show ([1 ..] !! vars s)
  return $ case t of
    E -> Var.Evidence $ "e" ++ count
    U -> Var.Unification ("v" ++ count) (Level $ level s) K.Kind
    T -> Var.Type ("p" ++ count) K.Kind


instance Instantiate Kind  where
  instantiate qt@(Forall [] _) t = qt
  instantiate (Forall (tvar:tvars) qt) t = Forall tvars qt'
    where
      sub = Map.fromList [(tvar, t)]
      qt' = apply sub qt

instance Generalize Kind where
    generalize k = return $ Forall (Set.toList $ ftv k) ([] :=> k)

class HasKind t where
  kind :: t -> KindInference Kind

instance HasKind Type where
  kind (T.Data _ k) = return k
  kind (T.Var v)    = kind v
  kind (T.Applied f _) = do
    k <- kind f
    case k of
      (K.Arrow _ k') -> return k'
      k'             -> Eff.throwError $ UnexpectedKind k' "Tried to apply a type to a non Arrow Kind"

  kind (T.Closure ps tyExpr closure) = do
    kvar <- fresh' T
    foldrM mkArrow (K.Var kvar) ps
    where
        mkArrow v k = do
            k' <- kind v
            return $ K.Arrow k' k

  kind  _ = return K.Type


instance HasKind (PolymorphicVar Type) where
    kind (Var.Type _ k)       = return k
    kind (Skolem _ k)         = return k
    kind (Unification _ _ k)  = return k
    kind i@(Instantiation {}) = Eff.throwError $ UnexpectedInstantiationVariable i
