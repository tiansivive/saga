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

import qualified Data.Set                                      as Set
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
import           Saga.Language.Typechecker.TypeExpr            (TypeAtom,
                                                                TypeExpr)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables

type instance VarType TypeExpr I.Evidence       = CST.Evidence
type instance VarType TypeExpr I.Unification    = Var.PolymorphicVar Kind
type instance VarType TypeExpr I.Skolem         = Var.PolymorphicVar Kind
type instance VarType TypeExpr I.PolyVar        = Var.PolymorphicVar Kind
type instance VarType TypeExpr I.Instantiation  = Var.PolymorphicVar Kind


type instance I.Constraint Kind = UnificationConstraint

data UnificationConstraint = Empty | Unify Kind Kind
type KindInference m = InferM UnificationConstraint m

-- | Inferring Kinds of Types
instance Inference TypeExpr where

    infer = infer'
    lookup = lookup'
    fresh = fresh'



infer' :: KindInference m => TypeExpr -> m TypeExpr
infer' te = case te of
    TE.Atom atom -> inferAtom atom
    TE.Implementation p tyExpr -> infer tyExpr
    TE.Tagged tag tyExpr -> do
        (TE.KindedType ty k) <- infer tyExpr
        return $ TE.KindedType ty (K.Data tag k)

    TE.Lambda ps@(param : rest) body -> do
        kVar <- fresh' U
        name' <- name param
        let qk = Forall [] (pure $ K.Var kVar)
        let scoped = local (\e -> e { kinds = Map.insert name' qk $ kinds e })
        scoped $ do
            o@(TE.KindedType _ out') <- infer out
            let k = K.Var kVar `K.Arrow` out'
            let tyExpr = TE.Lambda ps o
            return $ TE.KindedType tyExpr k
        where
            out = case rest of
              [] -> body
              _  -> TE.Lambda rest body
            name (PolyVar id _) = return id
            name v              = throwError $ UnexpectedPolymorphicVariable v

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

    where
        inferAtom ::  KindInference m => TypeAtom -> m TypeExpr
        inferAtom atom = case atom of
            TE.Identifier id -> do
                _ :=> k <- lookup' id
                return $ TE.KindedType (TE.Atom atom) k
            TE.Arrow in' out' -> do
                kindedIn <- infer' in'
                kindedOut <- infer' out'
                return $ TE.KindedType (TE.Atom $ kindedIn `TE.Arrow` kindedOut) K.Type
            _ -> return $ TE.KindedType (TE.Atom atom) K.Type


lookup' :: KindInference m => String -> m (Qualified Kind)
lookup' x = do
  Saga { kinds } <- ask
  case Map.lookup x kinds of
    Just scheme -> inst scheme
    Nothing     -> throwError $ UnboundVariable x

  where
    inst scheme@(Forall [] qk) = return qk
    inst scheme@(Forall tvars _) = fresh' U >>= instantiate scheme . K.Var >>= inst



fresh' :: InferM w m => Tag a -> m (VarType TypeExpr a)
fresh' t = do
  modify $ \s -> s {vars = vars s + 1}
  s <- get
  let count = show ([1 ..] !! vars s)
  return $ case t of
    E -> CST.Var $ "e" ++ count
    U -> Var.Unification ("v" ++ count) (Level $ level s) K.Kind
    P -> Var.PolyVar ("p" ++ count) K.Kind


instance Instantiate Kind  where
  instantiate qt@(Forall [] _) t = return qt
  instantiate (Forall (tvar:tvars) qt) t = return $ Forall tvars qt'
    where
      sub = Map.fromList [(tvar, t)]
      qt' = apply sub qt

instance Generalize Kind where
    generalize k = return $ Forall (Set.toList $ ftv k) ([] :=> k)

class HasKind t where
  kind :: InferM w m => t -> m Kind

instance HasKind Type where
  kind (T.Data _ k) = return k
  kind (T.Var v)    = kind v
  kind (T.Applied f _) = do
    k' <- kind f
    return $ case k' of
      (K.Arrow _ k) -> k
      _             -> error "Is this an error? Kind of T.Applied not a K.Arrow"

  kind (T.Closure ps tyExpr closure) = do
    kvar <- fresh' U
    foldrM mkArrow (K.Var kvar) ps
    where
        mkArrow v k = do
            k' <- kind v
            return $ K.Arrow k' k

  kind  _ = return K.Type


instance HasKind (PolymorphicVar Type) where
    kind (PolyVar _ k)        = return k
    kind (Skolem _ k)         = return k
    kind (Unification _ _ k)  = return k
    kind i@(Instantiation {}) = throwError $ UnexpectedInstantiationVariable i
