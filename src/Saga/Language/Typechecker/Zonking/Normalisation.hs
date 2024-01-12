{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}

module Saga.Language.Typechecker.Zonking.Normalisation where

import           Saga.Language.Typechecker.Solver.Substitution   (Substitutable (..))


import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe)
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           Effectful                                       (Eff, (:>))
import qualified Effectful.Reader.Static                         as Eff

import qualified Saga.Language.Syntax.Zonked                     as AST
import qualified Saga.Language.Typechecker.Kind                  as K
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (Type)
import qualified Saga.Language.Typechecker.Variables             as Var
import           Saga.Language.Typechecker.Variables             (Variable)
import           Saga.Language.Typechecker.Zonking.Substitutions


import           Control.Monad                                   (forM)
import           Data.Functor                                    ((<&>))
import qualified Effectful.Error.Static                          as Eff
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors                (Exception (NotYetImplemented),
                                                                  SagaError (UnexpectedVariable),
                                                                  crash)
import qualified Saga.Language.Typechecker.Qualification         as Q
import           Saga.Language.Typechecker.Qualification         (Given (..),
                                                                  Qualified (..))
import qualified Saga.Language.Typechecker.Solver.Constraints    as Solver
import           Saga.Language.Typechecker.TypeExpr              (TypeExpr)

type NormalizedEff es t = (TypeCheck es,  Eff.Reader [(Variable t, String)] :> es)
type Normalized t a = forall es. NormalizedEff es t => Eff es a
class Normalisation a where
    type Of a
    normalise :: NormalizedEff es (Of a) => a -> Eff es a

instance Normalisation Expr where
    type Of Expr = Type

    normalise =  \case
        AST.Typed e ty -> AST.Typed <$> normalise e <*> normalise ty

        AST.Lambda ps body -> AST.Lambda ps <$> normalise body
        AST.FnApp fn args  -> AST.FnApp <$> normalise fn <*> forM args normalise
        AST.Match cond cases -> AST.Match <$> normalise cond <*> forM cases normalise

        AST.Record es -> AST.Record <$> mapM (mapM normalise) es
        AST.Tuple es  -> AST.Tuple <$> mapM normalise es
        AST.List es   -> AST.List <$> mapM normalise es

        AST.Block stmts -> AST.Block <$> mapM normalise stmts
        e           -> return e


instance Normalisation AST.Case where
    type Of AST.Case = Type
    normalise = \case
        AST.Case pat e -> AST.Case pat <$> normalise e
        AST.TypedCase pat ty e -> AST.TypedCase pat <$> normalise ty <*> normalise e

instance Normalisation AST.Statement where
    type Of AST.Statement = Type
    normalise = \case
        AST.Return e -> AST.Return <$> normalise  e
        AST.Procedure e -> AST.Procedure <$> normalise e
        AST.Declaration d -> AST.Declaration <$> normalise d

instance Normalisation AST.Declaration where
    type Of AST.Declaration = Type
    normalise = \case
        AST.Let id ty k e -> AST.Let id ty k <$> normalise e
        d -> return d


instance Normalisation (T.Polymorphic Type) where
    type Of (T.Polymorphic Type) = Type

    normalise (T.Forall tvars qt) = do
        tvars' <- mapM normalise tvars
        qt' <- normalise qt
        return $ T.Forall tvars' qt'

instance Normalisation (Qualified Type) where
    type Of (Qualified Type) = Type
    normalise (bs :| cs :=> ty) = do
        bs' <- mapM normalise bs
        cs' <- mapM normalise cs
        ty' <- normalise ty
        return $ bs' :| cs' :=> ty'

instance Normalisation T.Constraint where
    type Of T.Constraint = Type

    normalise (Q.Equality ty ty') = Q.Equality <$> normalise ty <*> normalise ty'
    normalise (Q.Implements ty prtcl) = Q.Implements <$> normalise ty <*> pure prtcl
    normalise (Q.Refinement binding liquid ty) = Q.Refinement <$> normalise binding <*> pure liquid <*> normalise ty
    normalise (Q.Pure ty ) = Q.Pure <$> normalise ty
    normalise (Q.Resource m ty) = Q.Resource m <$> normalise ty

instance Normalisation (Q.Binding a) where
    type Of (Q.Binding a) = Type

    normalise = normalise

instance Normalisation Type where
    type Of Type = Type

    normalise = \case
        T.Var tvar                      -> T.Var <$> normalise tvar
        T.Tuple elems                   -> T.Tuple <$> mapM normalise elems
        T.Record pairs                  -> T.Record <$> mapM (mapM normalise) pairs
        T.Union elems                   -> T.Union <$> mapM normalise elems
        T.Arrow arg out                 -> T.Arrow <$> normalise arg <*> normalise out
        T.Applied con arg               -> T.Applied <$> normalise con <*> normalise arg
        T.Closure params tyExpr scope   -> T.Closure <$> mapM normalise params <*> pure tyExpr <*> pure scope
        t                               -> return t

instance Normalisation (Variable Type) where
    type Of (Variable Type) = Type

    normalise tvar = do
        mapping <- Eff.ask
        replaced <- sequence $ lookup tvar mapping <&> \id -> case tvar of
                (T.Poly _ k)        -> return $ T.Poly id k
                (T.Unification _ k) -> return $ T.Poly id k
                (T.Local _ k)       -> return $ T.Local id k
                (T.Scoped _ k)      -> return $ T.Local id k
                (T.Rigid _ k)       -> return $ T.Rigid id k
                (T.Skolem _ k)      -> return $ T.Skolem id k
                v                   -> Eff.throwError $ UnexpectedVariable v

        return $ fromMaybe tvar replaced



instance Normalisation Solver.Constraint where
    type Of Solver.Constraint = Type

    normalise = \case
        Solver.Impl ev (Solver.Mono ty) pid -> do
            ty' <- normalise ty
            return $ Solver.Impl ev (Solver.Mono ty') pid
        Solver.Refined scope (Solver.Mono ty) liquid -> do
            ty' <- normalise ty
            scope' <- mapM normalise scope
            return $ Solver.Refined scope' (Solver.Mono ty') liquid

        c -> return c


instance Normalisation Solver.Item where
    type Of Solver.Item = Type

    normalise (Solver.Mono ty)  = Solver.Mono <$> normalise ty
    normalise (Solver.Poly ty)  = Solver.Poly <$> normalise ty
    normalise (Solver.Var tvar) = Solver.Var <$> normalise tvar


    -- normalise it = crash . NotYetImplemented $ "Normalisation for item: " ++ show it
