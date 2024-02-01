{-# LANGUAGE ViewPatterns #-}
module Saga.Language.Typechecker.Solving.Unification where
import           Control.Monad                                 (foldM, forM,
                                                                liftM2,
                                                                zipWithM,
                                                                zipWithM_)
import           Data.Functor                                  ((<&>))
import qualified Data.List                                     as List
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (catMaybes)
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Syntax.AST                      (Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.AST           as AST
import qualified Saga.Language.Syntax.Elaborated.Kinds         as K
import qualified Saga.Language.Syntax.Elaborated.Types         as T
import           Saga.Language.Syntax.Elaborated.Types         (Type)
import           Saga.Language.Syntax.Literals                 (Literal (..))
import           Saga.Language.Typechecker.Env                 (CompilerState (kinds, types))
import           Saga.Language.Typechecker.Errors              (Exception (..),
                                                                SagaError (..),
                                                                crash)
import qualified Saga.Language.Typechecker.Evaluation.Types    as E
import qualified Saga.Language.Typechecker.Lib                 as Lib
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Solving.Cycles      (Cycle)
import           Saga.Language.Typechecker.Solving.Monad       (Proofs, Solving)
import qualified Saga.Language.Typechecker.Solving.Shared      as Shared
import           Saga.Language.Typechecker.Solving.Shared      (Tag (..))
import           Saga.Language.Typechecker.Substitution        (Subst,
                                                                Substitutable (..),
                                                                compose,
                                                                nullSubst)
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Utils.Common                             (distribute2)
import           Saga.Utils.Operators                          ((|>), (||>))


type UnificationEff es t = (Solving es, Eff.Writer [Cycle t] :> es, Eff.Writer Proofs :> es, Eff.Writer Solver.Constraint :> es)



class Substitutable t => Unification t where
    unify       :: UnificationEff es t => t -> t -> Eff es (Subst t)
    bind        :: UnificationEff es t => Variable t -> t -> Eff es (Subst t)
    occursCheck :: Variable t -> t -> Bool



type TypeUnification es = UnificationEff es Type
instance Unification Type where
    unify :: TypeUnification es => Type -> Type -> Eff es (Subst Type)
    --unify t t' | pTrace ("\nUnifying:\n" ++ show t ++ "\nWith:\n" ++ show t') False = undefined
    unify (T.Var v) t
      | T.Instantiation {} <- v   = Eff.throwError $ UnexpectedVariable v
      | T.Unification {} <- v   = bind v t
      | T.Scoped {} <- v        = bind v t
      | T.Local {} <- v         = bind v t
      | T.Poly {} <- v          = bind v t
      | T.Skolem {} <- v
      , T.Var v' <- t             = bind v' (T.Var v)
      | T.Rigid {} <- v
      , T.Var v' <- t             = bind v' (T.Var v)
      | T.Skolem {} <- v = Eff.throwError $ RigidUnification v t
      | T.Rigid {} <- v = Eff.throwError $ RigidUnification v t
      | otherwise = crash $ NotYetImplemented $ "Binding avar:\n" ++ show v ++ "\nwith\n" ++ show t

    unify t (T.Var v)
      | T.Instantiation {} <- v  = Eff.throwError $ UnexpectedVariable v
      | T.Unification {} <- v   = bind v t
      | T.Scoped {} <- v        = bind v t
      | T.Local {} <- v         = bind v t
      | T.Poly {} <- v          = bind v t
      | T.Skolem {} <- v
      , T.Var v' <- t           = bind v' (T.Var v)
      | T.Rigid {} <- v
      , T.Var v' <- t           = bind v' (T.Var v)
      | T.Skolem {} <- v    = Eff.throwError $ RigidUnification v t
      | T.Rigid {} <- v     = Eff.throwError $ RigidUnification v t
      | otherwise           = crash $ NotYetImplemented $ "Binding avar:\n" ++ show v ++ "\nwith\n" ++ show t


    unify t@(T.Singleton a) t'@(T.Singleton b)  | a == b            = return nullSubst
                                                | otherwise         = Eff.throwError $ UnificationFail t t'
    unify t@(T.Data con ) t'@(T.Data con')      | con == con'       = return nullSubst
                                                | otherwise         = Eff.throwError $ UnificationFail t t'

    unify (T.Tuple as) (T.Tuple bs) = do
        zipWithM_ emitKindEquality kas kbs
        ss <- zipWithM unify tas tbs
        return $ foldl compose nullSubst ss

        where
            (tas, kas) = as ||> fmap (distribute2 AST.node AST.annotation) |> unzip
            (tbs, kbs) = bs ||> fmap (distribute2 AST.node AST.annotation) |> unzip

    unify (il `T.Arrow` ol) (ir `T.Arrow` or) = do
        Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev (project inLeftKind) (project inRightKind)
        Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev (project outLeftKind) (project outRightKind)

        sub <- unify inLeftType inRightType
        s <- unify (apply sub outLeftType) (apply sub outRightType)
        return $ s `compose` sub

        where
            (inLeftType, inLeftKind) = il ||> distribute2 AST.node AST.annotation
            (inRightType, inRightKind) = ir ||> distribute2 AST.node AST.annotation
            (outLeftType, outLeftKind) = ol ||> distribute2 AST.node AST.annotation
            (outRightType, outRightKind) = or ||> distribute2 AST.node AST.annotation

    unify (T.Applied f t) (T.Applied f' t') = do
        Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev (project fKind) (project fKind')
        Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev (project tKind) (project tKind')

        sub <- unify fType fType'
        s <-  unify (apply sub tType) (apply sub tType')
        return $ s `compose` sub

        where
            (fType, fKind) = f ||> distribute2 AST.node AST.annotation
            (tType, tKind) = t ||> distribute2 AST.node AST.annotation
            (fType', fKind') = f' ||> distribute2 AST.node AST.annotation
            (tType', tKind') = t' ||> distribute2 AST.node AST.annotation


    unify sub@(T.Record as)   parent@(T.Record bs)  = sub `isSubtype` parent
    unify lit@(T.Singleton _) prim@(T.Data _ )      = lit `isSubtype` prim


    unify u1@(T.Union nodes1) u2@(T.Union nodes2)  = do
        subs <- forM pairs1 $ \pair1 -> catMaybes <$> forM pairs2 (unifier pair1)

        if not $ any null subs then
          return $ foldl (foldl compose) nullSubst subs
        else Eff.throwError $ UnificationFail u1 u2

        where
            unifier t1 t2 = Eff.catchError @SagaError (unify' t1 t2 <&> Just) (\_ _ -> return Nothing)
            unify' (t1, project -> k1) (t2, project -> k2) = do
                Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev k1 k2
                unify t1 t2

            pairs1 = nodes1 ||> fmap (distribute2 AST.node AST.annotation)
            pairs2 = nodes2 ||> fmap (distribute2 AST.node AST.annotation)


    unify (T.Union nodes) t2 = foldM unifier nullSubst tys
      where
        tys = nodes ||> fmap AST.node
        unifier sub t1 = compose sub <$> unify (apply sub t1) t2

    unify t1 t2@(T.Union nodes) = do
      subs <- catMaybes <$> mapM (unifier t1) tys
      if null subs then
        Eff.throwError $ UnificationFail t1 t2
      else return $ foldl compose nullSubst subs
      where
        tys = nodes ||> fmap AST.node
        unifier t1 t2 = Eff.catchError @SagaError (unify t1 t2 <&> Just) (\_ _ -> return Nothing)

    unify t t' = do
        -- | NOTE: The kind stuff below should not apply anymore since we're elaborating kinds and emitting and solving kind constraints
        -- ((k, k'), cs) <- KI.run kinds

        -- -- | QUESTION: Should we intercept the error here to add extra context? eg. Eff.throwError $ KindMismatch k k'
        -- (subst, cycles) <- Eff.runWriter @[Cycle K.Kind] $ unify k k'
        -- unless (null cycles) (Eff.Eff.throwError $ CircularKind k k')

        case (t, t') of
          (t1@(T.Closure {}), t2) -> do
            --Forall tvars (cs :=> t1') <- eval t1
            foo <- eval t1
            unify _t1' t2
          (t1, t2@(T.Closure {})) -> do
            ---Forall tvars (cs :=> t2') <- eval t2
            unify t1 _t2'
          _ -> crash $ NotYetImplemented $ "Unifying types: " ++ show t ++ " and " ++ show t'

        where
        --   kinds :: _KindInference es => Eff es (K.Kind, K.Kind)
        --   kinds = do
        --     k  <- kind t
        --     k' <- kind t'
        --     return (k, k')



          eval (T.Closure tyExpr captured) = Eff.local @(CompilerState Elaborated) (extend captured) $ E.evaluate tyExpr
          extend (T.Scope tys ks) env = env { types = tys `Map.union` types env
                                            , kinds = ks `Map.union` kinds env
                                            }
            -- where
                -- params' = foldr (\p@(T.Poly id _) -> Map.insert id $ Forall [] (Q.none :=> T.Var p)) Map.empty params
                --extended env = env{ types = params' `Map.union` T.types captured `Map.union` types env }


    bind a t
        | t == T.Var a = return nullSubst
        | occursCheck a t = case t of
            T.Union (fmap AST.node -> tys)
                | T.Var a `elem` tys  -> do
                    let tys' = List.delete (T.Var a) tys
                    let solution = Map.singleton a $ T.Union (fmap AST.Raw tys' )
                    Eff.tell [(a, t, solution)]
                    return nullSubst

        | otherwise = do
            -- | NOTE: The kind stuff below should not apply anymore since we're elaborating kinds and emitting and solving kind constraints

            -- -- | QUESTION: Should we propagate constraints here?
            -- (tk, cs) <- KI.run $ kind t
            -- let ak = T.classifier a
            -- -- | QUESTION: Should we intercept the error here to add extra context? eg. Eff.throwError $ KindMismatch k k'
            -- (subst, cycles) <- Eff.runWriter @[Cycle K.Kind] $ unify ak tk
            -- unless (null cycles) (Eff.Eff.throwError $ CircularKind ak tk)



            -- | QUESTION: How to remove the double cases .. of check on `t` here?
            case t of
              T.Singleton lit -> Eff.tell $ Map.singleton a lit
              _               -> return ()

            return . Map.singleton a $ case t of
                T.Singleton (LInt _)    -> T.Data "Int"
                T.Singleton (LString _) -> T.Data "String"
                T.Singleton (LBool _)   -> T.Data "Boolean"
                _                       -> t



    occursCheck a t = a `Set.member` set
        where set = ftv t

isSubtype :: TypeUnification es => Type -> Type -> Eff es (Subst Type)
T.Singleton (LInt _) `isSubtype` int | int == T.Data "Int"        = return nullSubst
T.Singleton (LString _) `isSubtype` str | str == T.Data "String"  = return nullSubst
T.Singleton (LBool _) `isSubtype` bool | bool == T.Data "Boolean" = return nullSubst

sub@(T.Record pairs1) `isSubtype` parent@(T.Record pairs2) = do
  subs <- mapM check pairs2
  return $ foldl compose nullSubst subs
  where
    check (name, distribute2 AST.node AST.annotation -> (t2, k2)) = case lookup name pairs1 of
        Nothing  -> Eff.throwError $ UnificationFail sub parent
        Just (distribute2 AST.node AST.annotation -> (t1, k1)) -> do
            Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev (project k1) (project k2)
            unify t1 t2


sub `isSubtype` parent = Eff.throwError $ SubtypeFailure sub parent



project = Solver.K . AST.node
emitKindEquality (project -> ka) (project -> kb) = Shared.fresh E >>= \ev -> Eff.tell $ Solver.Equality ev ka kb
