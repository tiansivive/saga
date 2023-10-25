
module Saga.Language.Typechecker.Solver.Unification where
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set

import           Saga.Language.Typechecker.Variables           (PolymorphicVar (..))


import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Functor                                  ((<&>))
import           Data.Maybe                                    (catMaybes)
import           Saga.Language.Core.Literals                   (Literal (..))
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import qualified Saga.Language.Typechecker.Kind                as K

import           Control.Monad.Trans.Reader                    (ReaderT (runReaderT))
import           Control.Monad.Trans.Writer                    (runWriterT)
import qualified Saga.Language.Typechecker.Evaluation          as E
import           Saga.Language.Typechecker.Inference.Inference (InferM,
                                                                State (..),
                                                                emit', inform')
import           Saga.Language.Typechecker.Inference.Kind      (HasKind (..))
import           Saga.Language.Typechecker.Kind                (Kind)
import           Saga.Language.Typechecker.Solver.Substitution (Subst, apply,
                                                                compose, ftv,
                                                                nullSubst)
import qualified Saga.Language.Typechecker.Type                as T


import qualified Data.List                                     as List
import           Saga.Language.Typechecker.Qualification       (Qualified ((:=>)))
import           Saga.Language.Typechecker.Shared              (classifier)
import           Saga.Language.Typechecker.Type                (Scheme (..),
                                                                Type)




type UnificationM t m = InferM (Cycle t) m
type TypeUnification m = UnificationM Type m

type Cycle t = (PolymorphicVar t, t, Subst t)

class Unification t where
    unify       :: UnificationM t m => t -> t -> m (Subst t)
    bind        :: UnificationM t m => PolymorphicVar t -> t -> m (Subst t)
    occursCheck :: PolymorphicVar t -> t -> Bool



instance Unification Type where
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
        --traceM $ ("\nAll subs:\n\t" ++ show subs)
        if not $ any null subs then
          return $ foldl (foldl compose) nullSubst subs
        else throwError $ UnificationFail u1 u2
        where
          unifier t1 t2 = catchError (unify t1 t2 <&> Just) (\_ -> return Nothing)

    unify t1@(T.Union tys) t2 = foldM unifier nullSubst tys
      where
        unifier sub t = compose sub <$> unify (apply sub t) t2
    unify t1 t2@(T.Union tys) = do
      subs <- catMaybes <$> mapM (unifier t1) tys
      if null subs then
        throwError $ UnificationFail t1 t2
      else return $ foldl compose nullSubst subs
      where
        unifier t1 t2 = catchError (unify t1 t2 <&> Just) (\_ -> return Nothing)

    unify t t' = do
        k  <- kind t
        k' <- kind t'
        if k /= k' then
            throwError $ KindMismatch k k'
        else case (t, t') of
          (t1@(T.Closure {}), t2) -> do
            Forall tvars (cs :=> t1') <- eval t1
            unify t1' t2
          (t1, t2@(T.Closure {})) -> do
            Forall tvars (cs :=> t2') <- eval t2
            unify t1 t2'

        where
            eval (T.Closure params tyExpr captured) = do
                env <- ask
                (ty, info) <- E.run tyExpr (extended env) throwError
                inform' info
                return ty

                where
                    params' = foldr (\p@(PolyVar id _) -> Map.insert id $ Forall [] (pure $ T.Var p)) Map.empty params
                    extended env = env{ types = params' `Map.union` T.types captured `Map.union` types env }


    bind a t
        | t == T.Var a = return nullSubst
        | occursCheck a t = case t of
            T.Union tys | T.Var a `elem` tys    -> do
                let tys' = List.delete (T.Var a) tys
                let solution = Map.singleton a $ T.Union tys'
                emit' (a, t, solution)
                return nullSubst

        -- | TODO: need to check if kinds unify
        -- | Left err <- Kinds.run (kind a) (kind t) = throwError $ Fail  $ "kinds do not match:\n\tTyvar: " ++ show a ++ "\n\tType: " ++ show t ++ "\nError:\n\t" ++ show err
        | T.Singleton l <- t = do
            k <- classifier a
            if k /= K.Type then
                throwError $ KindMismatch k K.Type
            else return . Map.singleton a $ case l of
                LInt _    -> T.Data "Int" K.Type
                LString _ -> T.Data "String" K.Type
                LBool _   -> T.Data "Bool" K.Type
        | otherwise = return $ Map.singleton a t

    occursCheck a t = a `Set.member` set
        where set = ftv t

isSubtype :: TypeUnification m => Type -> Type -> m (Subst Type)
T.Singleton (LInt _) `isSubtype` T.Data "Int" K.Type = return nullSubst
T.Singleton (LString _) `isSubtype` T.Data "String" K.Type = return nullSubst
T.Singleton (LBool _) `isSubtype` T.Data "Bool" K.Type = return nullSubst

sub@(T.Record pairs1) `isSubtype` parent@(T.Record pairs2) = do
  let check (name, ty2) = case lookup name pairs1 of
        Nothing  -> throwError $ UnificationFail sub parent
        Just ty1 -> unify ty1 ty2
  subs <- mapM check pairs2
  return $ foldl compose nullSubst subs

sub `isSubtype` parent = throwError $ SubtypeFailure sub parent




instance Unification Kind where
    unify k k' = error "Kind unification not implemented yet"
