{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.Language.Core.Expr where

import           Saga.Language.Core.Literals        (Literal (LInt))



import           Saga.Language.Typechecker.Kind
import           Saga.Language.Typechecker.Type     as T hiding (Case)
import           Saga.Language.Typechecker.TypeExpr (TypeExpr)

-- | Term expressions
data Expr where
  Literal :: Literal -> Expr
  Identifier :: String -> Expr
  Hole :: String -> Expr
  List :: [Expr] -> Expr
  Tuple :: [Expr] -> Expr
  Record :: [(String, Expr)] -> Expr

  Match :: Expr -> [Case] -> Expr
  Lambda :: [String] -> Expr -> Expr
  FnApp :: Expr -> [Expr] -> Expr

  Block :: [Statement] -> Expr
  Typed :: Expr -> T.Type -> Expr



  --FieldAccess :: Expr -> String -> Expr
deriving instance Show Expr
deriving instance Eq Expr



data Binding a
  = Bind String a
  deriving (Show, Eq)


data Statement where
  Return      :: Expr -> Statement
  Declaration :: Declaration -> Statement
  Procedure   :: Expr -> Statement

deriving instance Show Statement
deriving instance Eq Statement



data Case
  = Case Pattern Expr
  | TypedCase Pattern Type Expr
  deriving (Show, Eq)

data Pattern
    = Wildcard
    | Id String
    | PatHole String
    | Lit Literal
    | PatTuple [Pattern] (Maybe String)
    | PatList [Pattern] (Maybe String)
    | PatRecord [(String, Maybe Pattern)] (Maybe String)
    | PatData String [Pattern]
    | TypedPat Pattern Type
  deriving (Show, Eq)

type DataExpr = (String, TypeExpr)

data Declaration
    = Let String (Maybe TypeExpr) (Maybe Kind) Expr
    | Type String (Maybe Kind) TypeExpr
    | Data String (Maybe Kind) TypeExpr
    deriving (Show, Eq)

newtype Script = Script [Declaration]
    deriving (Show, Eq)

