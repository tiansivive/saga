{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Reduced.Values where

import qualified Saga.Language.Syntax.AST           as NT (NodeType (..))
import           Saga.Language.Syntax.AST


import           Saga.Language.Syntax.Reduced.AST
import           Saga.Language.Syntax.Reduced.Kinds
import           Saga.Language.Syntax.Reduced.Types

import           Saga.Language.Syntax.Literals
import           Saga.Utils.TypeLevel               (type (§))

type Expr = Node Reduced NT.Expression
data instance Node Reduced Expression where
  Var         :: String -> Node Reduced Expression
  Hole        :: String -> Node Reduced Expression
  Literal     :: Literal -> Node Reduced Expression
  List        :: [AST Reduced Expression] -> Node Reduced Expression
  Tuple       :: [AST Reduced Expression] -> Node Reduced Expression
  Record      :: [(String,  AST Reduced Expression)] -> Node Reduced Expression
  Match       :: AST Reduced Expression -> [AST Reduced (NT.Case Expression)] -> Node Reduced Expression
  Lambda      :: [String] -> AST Reduced Expression ->  Node Reduced Expression
  Application ::  AST Reduced Expression -> [AST Reduced Expression] -> Node Reduced Expression
  Block       :: [AST Reduced Statement] -> Node Reduced Expression
deriving instance Show (Node Reduced NT.Expression)
deriving instance Show (AST Reduced NT.Expression)


data instance Node Reduced Statement where
  Return       :: AST Reduced Expression ->  Node Reduced Statement
  Procedure    :: AST Reduced Expression ->  Node Reduced Statement
  -- TODO:SUGGESTION turn declarations into Let bindings (Let .. in) expressions, which would require a new data constructor for expressions
  Declaration  :: Node Reduced NT.Declaration ->  Node Reduced Statement
deriving instance Show (Node Reduced NT.Statement)
deriving instance Show (AST Reduced NT.Statement)


data instance Node Reduced NT.Declaration where
  Let  :: String -> AST Reduced Expression    -> Node Reduced NT.Declaration
  Type :: String -> AST Reduced NT.Type -> Node Reduced NT.Declaration
  Kind :: String -> AST Reduced NT.Kind -> Node Reduced NT.Declaration
deriving instance Show (Node Reduced NT.Declaration)


data instance Node Reduced (NT.Case Expression) where
  Case :: AST Reduced (Pattern NT.Expression) -> AST Reduced NT.Expression -> Node Reduced (NT.Case NT.Expression)
deriving instance Show (Node Reduced (NT.Case Expression))
deriving instance Show (AST Reduced (NT.Case Expression))

data instance Node Reduced (Pattern Expression) where
  Wildcard  :: Node Reduced (Pattern Expression)
  Id        :: String -> Node Reduced (Pattern Expression)
  PatHole   :: String -> Node Reduced (Pattern Expression)
  PatLit    :: Literal  -> Node Reduced (Pattern Expression)
  PatTuple  :: [AST Reduced (Pattern Expression)] -> Maybe String  -> Node Reduced (Pattern Expression)
  PatList   :: [AST Reduced (Pattern Expression)] -> Maybe String  -> Node Reduced (Pattern Expression)
  PatRecord :: [(String, Maybe § AST Reduced (Pattern Expression))] -> Maybe String  -> Node Reduced (Pattern Expression)
  PatData   :: String -> [AST Reduced (Pattern Expression)]  -> Node Reduced (Pattern Expression)
deriving instance Show (Node Reduced (NT.Pattern Expression))
deriving instance Show (AST Reduced (NT.Pattern Expression))
