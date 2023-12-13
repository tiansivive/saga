{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}


module Saga.Language.Typechecker.Solver.Unification where
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set

import           Saga.Language.Typechecker.Variables           (Variable (..))



import           Control.Monad.RWS
import           Data.Functor                                  ((<&>))
import           Data.Maybe                                    (catMaybes)
import           Saga.Language.Core.Literals                   (Literal (..))
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (Exception (NotYetImplemented),
                                                                SagaError (..),
                                                                crash)
import qualified Saga.Language.Typechecker.Kind                as K

import           Control.Monad.Trans.Reader                    (ReaderT (runReaderT))
import           Control.Monad.Trans.Writer                    (WriterT,
                                                                runWriterT)
import qualified Saga.Language.Typechecker.Evaluation          as E


import           Saga.Language.Typechecker.Inference.Kind      (HasKind (..),
                                                                KindInference)
import           Saga.Language.Typechecker.Kind                (Kind)
import           Saga.Language.Typechecker.Solver.Substitution (Subst,
                                                                Substitutable,
                                                                apply, compose,
                                                                ftv, nullSubst)
import qualified Saga.Language.Typechecker.Type                as T


import           Control.Monad.Writer                          (Writer)
import qualified Data.Kind                                     as K
import qualified Data.List                                     as List
import qualified Saga.Language.Typechecker.Lib                 as Lib
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import           Saga.Language.Typechecker.Qualification       (Qualified ((:=>)))
import           Saga.Language.Typechecker.Type                (Scheme (..),
                                                                Type)
import qualified Saga.Language.Typechecker.Variables           as Var

import           Debug.Pretty.Simple                           (pTrace)
import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import           Effectful.Error.Static                        (catchError,
                                                                throwError)
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.State.Static.Local                  as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import qualified Saga.Language.Typechecker.Inference.Kind      as KI
import qualified Saga.Language.Typechecker.Qualification       as Q
import           Saga.Language.Typechecker.Solver.Cycles       (Cycle)


type UnificationEff es t = (TypeCheck es, Eff.Writer [Cycle t] :> es)
type UnificationM t a = forall es. UnificationEff es t => Eff es a


class Substitutable t => Unification t where
    unify       :: UnificationEff es t => t -> t -> Eff es (Subst t)
    bind        :: UnificationEff es t => Variable t -> t -> Eff es (Subst t)
    occursCheck :: Variable t -> t -> Bool



type TypeUnification es = UnificationEff es Type
instance Unification Type where
    unify :: TypeUnification es => Type -> Type -> Eff es (Subst Type)
--    unify t t' | pTrace ("\nUnifying:\n\t" ++ show t ++ "\n\t" ++ show t') False = undefined
    unify (T.Var v) t                                               = bind v t
    unify t (T.Var v)                                               = bind v t
    unify (T.Singleton a) (T.Singleton b) | a == b                  = return nullSubst
    unify (T.Data con K.Type) (T.Data con' K.Type) | con == con'    = return nullSubst

    unify (T.Tuple as) (T.Tuple bs) = do
          ss <- zipWithM unify as bs
          return $ foldl compose nullSubst ss
    unify (il `T.Arrow` ol) (ir `T.Arrow` or) = do
        sub <- unify il ir
        s <- unify (apply sub ol) (apply sub or)
        return $ s `compose` sub
    unify (T.Applied f t) (T.Applied f' t') = do
        sub <- unify f f'
        s <-  unify (apply sub t) (apply sub t')
        return $ s `compose` sub

    unify sub@(T.Record as)   parent@(T.Record bs) = sub `isSubtype` parent
    unify lit@(T.Singleton _) prim@(T.Data _ K.Type) = lit `isSubtype` prim


    unify u1@(T.Union tys1) u2@(T.Union tys2)  = do
        subs <- forM tys1 $ \t1 -> forM tys2 (unifier t1) <&> catMaybes

        if not $ any null subs then
          return $ foldl (foldl compose) nullSubst subs
        else throwError $ UnificationFail u1 u2
        where
          unifier t1 t2 = catchError @SagaError (unify t1 t2 <&> Just) (\_ _ -> return Nothing)

    unify t1@(T.Union tys) t2 = foldM unifier nullSubst tys
      where
        unifier sub t = compose sub <$> unify (apply sub t) t2
    unify t1 t2@(T.Union tys) = do
      subs <- catMaybes <$> mapM (unifier t1) tys
      if null subs then
        throwError $ UnificationFail t1 t2
      else return $ foldl compose nullSubst subs
      where
        unifier t1 t2 = catchError @SagaError (unify t1 t2 <&> Just) (\_ _ -> return Nothing)

    unify t t' = do
        -- | QUESTION: Should we propagate constraints here?
        ((k, k'), cs) <- KI.run kinds

        -- | QUESTION: Should we intercept the error here to add extra context? eg. throwError $ KindMismatch k k'
        (subst, cycles) <- Eff.runWriter @[Cycle Kind] $ unify k k'
        unless (null cycles) (Eff.throwError $ CircularKind k k')

        case (t, t') of
          (t1@(T.Closure {}), t2) -> do
            Forall tvars (cs :=> t1') <- eval t1
            unify t1' t2
          (t1, t2@(T.Closure {})) -> do
            Forall tvars (cs :=> t2') <- eval t2
            unify t1 t2'

        where
          kinds :: KindInference es => Eff es (Kind, Kind)
          kinds = do
            k  <- kind t
            k' <- kind t'
            return (k, k')



          eval (T.Closure params tyExpr captured) = do
            let scoped =  Eff.local extended
            Eff.inject $ scoped $ E.evaluate tyExpr

            where
                params' = foldr (\p@(T.Poly id _) -> Map.insert id $ Forall [] (Q.none :=> T.Var p)) Map.empty params
                extended env = env{ types = params' `Map.union` T.types captured `Map.union` types env }


    bind a t
        | T.Local id k <- a = crash $ NotYetImplemented "Unifying and Binding locally scoped type vars"
        | t == T.Var a = return nullSubst
        | occursCheck a t = case t of
            T.Union tys | T.Var a `elem` tys    -> do
                let tys' = List.delete (T.Var a) tys
                let solution = Map.singleton a $ T.Union tys'
                Eff.tell [(a, t, solution)]
                return nullSubst

        | otherwise = do
            -- | QUESTION: Should we propagate constraints here?
            (tk, cs) <- KI.run $ kind t
            let ak = T.classifier a

           -- | QUESTION: Should we intercept the error here to add extra context? eg. throwError $ KindMismatch k k'
            (subst, cycles) <- Eff.runWriter @[Cycle Kind] $ unify ak tk
            unless (null cycles) (Eff.throwError $ CircularKind ak tk)

            return . Map.singleton a $ case t of
                T.Singleton (LInt _)    -> Lib.int
                T.Singleton (LString _) -> Lib.string
                T.Singleton (LBool _)   -> Lib.bool
                _                       -> t



    occursCheck a t = a `Set.member` set
        where set = ftv t

isSubtype :: TypeUnification es => Type -> Type -> Eff es (Subst Type)
T.Singleton (LInt _) `isSubtype` int | int == Lib.int       = return nullSubst
T.Singleton (LString _) `isSubtype` str | str == Lib.string = return nullSubst
T.Singleton (LBool _) `isSubtype` bool | bool == Lib.bool   = return nullSubst

sub@(T.Record pairs1) `isSubtype` parent@(T.Record pairs2) = do
  let check (name, ty2) = case lookup name pairs1 of
        Nothing  -> throwError $ UnificationFail sub parent
        Just ty1 -> unify ty1 ty2
  subs <- mapM check pairs2
  return $ foldl compose nullSubst subs

sub `isSubtype` parent = throwError $ SubtypeFailure sub parent



type KindUnification es = UnificationEff es Kind
instance Unification Kind where
    unify ::  KindUnification es => Kind -> Kind -> Eff es (Subst Kind)
    unify K.Type K.Type = return Map.empty
    unify K.Kind K.Kind = return Map.empty

    unify (K.Arrow inK1 outK1) (K.Arrow inK2 outK2) = do
        sub <- unify inK1 inK2
        s <- unify (apply sub outK1) (apply sub outK2)
        return $ s `compose` sub

    unify (K.Protocol k1) (K.Protocol k2)       = unify k1 k2
    unify (K.Constraint k1) (K.Constraint k2)   = unify k1 k2

    unify (K.Data id1 k1) (K.Data id2 k2) | id1 == id2 = unify k1 k2

    unify k (K.Var v)                    = bind v k
    unify (K.Var v) k                    = bind v k

    unify k1 k2                         = throwError $ UnificationKindFail k1 k2

    bind v k
        | K.Var v == k  = return Map.empty
        | occursCheck v k   = throwError $ InfiniteKind v k
        | otherwise     = return $ Map.singleton v k

    occursCheck v k = v `Set.member` ftv k




