{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Saga.Language.TypeSystem.Normalization where
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import qualified Data.Set                             as Set
import           Saga.Language.Core.Syntax
import           Saga.Language.TypeSystem.Constraints
import           Saga.Language.TypeSystem.Environment
import           Saga.Language.TypeSystem.Shared
import qualified Saga.Language.TypeSystem.Types       as T
import           Saga.Language.TypeSystem.Types
import           Saga.Utils.Utils                     ((|>), (||>))


class Normalized a where
  normalize :: Set.Set UnificationVar -> a -> a

instance Normalized TypeExpr where
  normalize ftv tyExpr = case tyExpr of
    (TQualified (cs :=> tyExpr')) -> TQualified $ (normConstraint <$> cs) :=> normalize ftv tyExpr'
    (TAtom ty)                    -> TAtom $ normalize ftv ty
    (TLambda params tyExpr')       -> TLambda (fmap norm' params) (normalize ftv tyExpr')
    (TTagged tag tyExpr')          -> TTagged tag $ normalize ftv tyExpr'
    _ -> tyExpr

    where
      ord = zip tvars letters
      tvars = ftv ||> Set.toList |> fmap (\(Tyvar v _) -> v)
      norm' x = fromMaybe x (lookup x ord)

      normConstraint (t `T.Implements` p) = normalize ftv t `T.Implements` p


instance Normalized Type where
  normalize ftv = normalize'
    where
      mapping = zip tvars letters
      tvars = ftv ||> Set.toList |> fmap (\(Tyvar v _) -> v)
      norm' x = fromMaybe x (lookup x mapping)

      normalize' = \case
        (TTuple tys)        -> TTuple $ fmap normalize' tys
        (TUnion tys)        -> TUnion . Set.fromList $ fmap normalize' (Set.toList tys)
        (TRecord pairs)     -> TRecord $ fmap (fmap normalize') pairs
        (TApplied cons arg) -> normalize' cons `TApplied` normalize' arg
        (a `TArrow` b)      -> normalize' a `TArrow` normalize' b
        (TVar (Tyvar v k))  -> TVar $ Tyvar (norm' v) k
        (TData (Tycon x k)) -> TData $ Tycon (norm' x) k
        t                   -> t

instance Normalized (Map.Map String Type) where
  normalize _ unifier = fmap (normalize tvars) unifier
    where
      vals = snd <$> Map.toList unifier
      tvars = foldl (\vars t -> vars <> ftv t) Set.empty vals

instance Normalized Expr where
  normalize ftv = normalize'
    where
      mapping = zip tvars letters
      tvars = ftv ||> Set.toList |> fmap (\(Tyvar v _) -> v)
      norm' x = fromMaybe x (lookup x mapping)

      normalize' = \case
        Typed e ty -> Typed (normalize' e) (normalize ftv ty)

        Lambda ps body -> Lambda (fmap norm' ps) (normalize' body)
        FnApp fn args  -> FnApp (normalize' fn) (fmap normalize' args)
        Match cond cases -> Match (normalize' cond) (fmap (normalize ftv) cases)

        Record es -> Record $ fmap normalize' <$> es
        Tuple es  -> Tuple $ fmap normalize' es
        List es   -> List $ fmap normalize' es

        Block stmts -> Block $ fmap (normalize ftv) stmts
        e           -> e

instance Normalized Case where
  normalize ftv = \case
    Case pat e -> Case pat (normalize ftv e)
    TypedCase pat ty e -> TypedCase pat (normalize ftv ty) (normalize ftv e)

instance Normalized Statement where
  normalize ftv = \case
    Return e -> Return $ normalize ftv e
    Procedure e -> Procedure $ normalize ftv e
    Declaration d -> Declaration $ normalize ftv d

instance Normalized Declaration where
  normalize ftv = \case
    Let id ty k e -> Let id (fmap (normalize ftv) ty) k (normalize ftv e)
    Type id k ty -> Type id k (normalize ftv ty)
    Data id k tyExpr -> Data id k (normalize ftv tyExpr)
