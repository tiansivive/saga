{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}

{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Core.Expr where

import           Saga.Language.Core.Literals (Literal (LInt))



import qualified Saga.Language.Core.AST      as NodeType (NodeType (..))
import           Saga.Language.Core.AST

import           Saga.Utils.TypeLevel        (type (§))

data instance AST Desugared e where
    Raw :: Node Desugared e -> AST Desugared e
    Annotated :: Node Desugared e -> Node Desugared (Annotation e) -> AST Desugared e


data instance  Node Desugared Expression where
  Var         :: String -> Node Desugared Expression
  Hole        :: String -> Node Desugared Expression
  Literal     :: Literal -> Node Desugared Expression
  List        :: [AST Desugared Expression] -> Node Desugared Expression
  Tuple       :: [AST Desugared Expression] -> Node Desugared Expression
  Record      :: [(String,  AST Desugared Expression)] -> Node Desugared Expression
  Match       ::  AST Desugared Expression -> [AST Desugared NodeType.Case] -> Node Desugared Expression
  Lambda      :: [String] ->   AST Desugared Expression ->  Node Desugared Expression
  Application ::  AST Desugared Expression -> [AST Desugared Expression] -> Node Desugared Expression
  Block       :: [AST Desugared Statement] -> Node Desugared Expression
deriving instance Show (Node Desugared NodeType.Expression)
deriving instance Show (AST Desugared NodeType.Expression)


data instance Node Desugared Statement where
  Return       :: AST Desugared Expression ->  Node Desugared Statement
  Procedure    :: AST Desugared Expression ->  Node Desugared Statement
  Declaration  :: AST Desugared NodeType.Declaration ->  Node Desugared Statement
deriving instance Show (Node Desugared NodeType.Statement)
deriving instance Show (AST Desugared NodeType.Statement)

data instance Node Desugared NodeType.Case where
  Case :: AST Desugared Pattern -> AST Desugared Expression -> Node Desugared NodeType.Case
deriving instance Show (Node Desugared NodeType.Case)
deriving instance Show (AST Desugared NodeType.Case)

data instance Node Desugared Pattern where
  Wildcard  :: Node Desugared Pattern
  Id        :: String -> Node Desugared Pattern
  PatHole   :: String -> Node Desugared Pattern
  PatLit    :: Literal  -> Node Desugared Pattern
  PatTuple  :: [AST Desugared Pattern] -> Maybe String  -> Node Desugared Pattern
  PatList   :: [AST Desugared Pattern] -> Maybe String  -> Node Desugared Pattern
  PatRecord :: [(String, Maybe § AST Desugared Pattern)] -> Maybe String  -> Node Desugared Pattern
  PatData   :: String -> [AST Desugared Pattern]  -> Node Desugared Pattern
deriving instance Show (Node Desugared NodeType.Pattern)
deriving instance Show (AST Desugared NodeType.Pattern)

data instance Node Desugared NodeType.Declaration where
  Let  :: String -> Node Desugared Expression    -> Node Desugared NodeType.Declaration
  Type :: String -> Node Desugared NodeType.Type -> Node Desugared NodeType.Declaration
  Kind :: String -> Node Desugared NodeType.Kind -> Node Desugared NodeType.Declaration
deriving instance Show (Node Desugared NodeType.Declaration)
deriving instance Show (AST Desugared NodeType.Declaration)

data instance Node Desugared NodeType.Type where

deriving instance Show (Node Desugared NodeType.Type)
deriving instance Show (AST Desugared NodeType.Type)

data instance Node Desugared NodeType.Kind where
deriving instance Show (Node Desugared NodeType.Kind)
deriving instance Show (AST Desugared NodeType.Kind)


-- | Term expressions
-- data Expr where
--   Literal :: Literal -> Expr
--   Identifier :: String -> Expr
--   Hole :: String -> Expr
--   List :: [Expr] -> Expr
--   Tuple :: [Expr] -> Expr
--   Record :: [(String, Expr)] -> Expr

--   Match :: Expr -> [Case] -> Expr
--   Lambda :: [String] -> Expr -> Expr
--   FnApp :: Expr -> [Expr] -> Expr

--   Block :: [Statement] -> Expr
--   Typed :: Expr -> T.Type -> Expr



--   --FieldAccess :: Expr -> String -> Expr
-- deriving instance Show Expr
-- deriving instance Eq Expr



-- data Binding a
--   = Bind String a
--   deriving (Show, Eq)


-- data Statement where
--   Return      :: Expr -> Statement
--   Declaration :: Declaration -> Statement
--   Procedure   :: Expr -> Statement

-- deriving instance Show Statement
-- deriving instance Eq Statement

-- data Pattern
--     = Wildcard
--     | Id String
--     | PatHole String
--     | Lit Literal
--     | PatTuple [Pattern] (Maybe String)
--     | PatList [Pattern] (Maybe String)
--     | PatRecord [(String, Maybe Pattern)] (Maybe String)
--     | PatData String [Pattern]
--     | TypedPat Pattern Type
--   deriving (Show, Eq)

-- type DataExpr = (String, TypeExpr)

-- data Declaration
--     = Let String (Maybe TypeExpr) (Maybe Kind) Expr
--     | Type String (Maybe Kind) TypeExpr
--     -- | Data String (Maybe Kind) TypeExpr
--     deriving (Show, Eq)

-- newtype Script = Script [Declaration]
--     deriving (Show, Eq)

