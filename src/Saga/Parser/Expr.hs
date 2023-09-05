{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.Parser.Expr where



import           Saga.Parser.Literals (Literal)
import qualified Saga.Parser.Types    as T
import           Saga.Parser.Types    hiding (Binding)


data Expr where
  Literal :: Literal -> Expr
  Identifier :: String -> Expr
  Hole :: String -> Expr
  List :: [Expr] -> Expr
  Tuple :: [Expr] -> Expr
  Record :: [(String, Expr)] -> Expr
  IfElse :: Expr -> Expr -> Expr -> Expr
  Match :: Expr -> [Case] -> Expr
  Lambda :: [String] -> Expr -> Expr
  FnApp :: Expr -> [Expr] -> Expr
  Clause :: Expr -> [Binding Expr] -> Expr

  Block :: [Statement] -> Expr
  Parens :: Expr -> Expr

deriving instance Show Expr
deriving instance Eq Expr


data Binding a
  = Bind String a
  deriving (Show, Eq)



data Statement where
  Return      :: Expr -> Statement
  BackCall    :: [Pattern] -> Expr -> Statement
  Declaration :: Declaration -> Statement
  Procedure   :: Expr -> Statement

deriving instance Show Statement
deriving instance Eq Statement



data Case = Case Pattern Expr
  deriving (Show, Eq)

data Pattern
    = Id String
    | Lit Literal
    | PatTuple [String]
    | PatList [String] (Maybe String)
    | PatRecord [String] (Maybe String)
    | PatData String [String]
  deriving (Show, Eq)



type DataExpr = (String, TypeExpr)

data Declaration
    = Let String (Maybe TypeExpr) (Maybe Kind) Expr
    | Type String (Maybe Kind) TypeExpr
    | Data String (Maybe Kind) [DataExpr] [T.Binding TypeExpr]
    deriving (Show, Eq)

newtype Script = Script [Declaration]
    deriving (Show, Eq)

