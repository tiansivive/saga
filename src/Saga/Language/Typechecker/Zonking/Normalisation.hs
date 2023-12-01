{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}

module Saga.Language.Typechecker.Zonking.Normalisation where
import           Saga.Language.Core.Expr                         (Expr)
import           Saga.Language.Typechecker.Solver.Substitution   (Substitutable (..))


import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe)
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           Effectful                                       (Eff)
import qualified Effectful.Reader.Static                         as Eff

import qualified Saga.Language.Core.Expr                         as AST
import qualified Saga.Language.Typechecker.Kind                  as K
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Shared                as Shared
import qualified Saga.Language.Typechecker.Type                  as T
import           Saga.Language.Typechecker.Type                  (Type)
import qualified Saga.Language.Typechecker.Variables             as Var
import           Saga.Language.Typechecker.Variables             (PolymorphicVar)
import           Saga.Language.Typechecker.Zonking.Substitutions


import           Data.Functor                                    ((<&>))
import qualified Effectful.Error.Static                          as Eff
import           Saga.Language.Typechecker.Environment
import           Saga.Language.Typechecker.Errors                (SagaError (UnexpectedVariable))
import qualified Saga.Language.Typechecker.Solver.Constraints    as Solver
import           Saga.Language.Typechecker.TypeExpr              (TypeExpr)

type Normalized t = TypeCheck '[Eff.Reader [(PolymorphicVar t, String)]]

class Normalisation a where
    type Of a
    normalise :: a -> Normalized (Of a) a

instance Normalisation Expr where
    type Of Expr = Type

    normalise =  \case
        AST.Typed e ty -> AST.Typed <$> normalise e <*> normalise ty

        AST.Lambda ps body -> AST.Lambda ps <$> normalise body
        AST.FnApp fn args  -> AST.FnApp <$> normalise fn <*> mapM normalise args
        AST.Match cond cases -> AST.Match <$> normalise cond <*> mapM normalise cases

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

instance Normalisation (PolymorphicVar Type) where
    type Of (PolymorphicVar Type) = Type

    normalise tvar = do
        mapping <- Eff.ask
        replaced <- sequence $ lookup tvar mapping <&> \id -> case tvar of
                (Var.Type _ k) -> return $ Var.Type id k
                (Var.Unification _ lvl k) -> return $ Var.Type id k

                v -> Eff.throwError $ UnexpectedVariable v

        return $ fromMaybe tvar replaced



instance Normalisation Solver.Constraint where
    type Of Solver.Constraint = Type

    normalise = \case
        Solver.Impl ev (Solver.Mono ty) pid -> do
            ty' <- normalise ty
            return $ Solver.Impl ev (Solver.Mono ty') pid
        c -> return c
