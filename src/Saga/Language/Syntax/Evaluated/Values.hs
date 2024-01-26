{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Evaluated.Values where

import qualified Saga.Language.Syntax.AST             as NT (NodeType (..))
import           Saga.Language.Syntax.AST


import           Saga.Language.Syntax.Evaluated.AST
import           Saga.Language.Syntax.Evaluated.Kinds
import           Saga.Language.Syntax.Evaluated.Types

import           Saga.Language.Syntax.Literals
import           Saga.Utils.TypeLevel                 (type (§))

type Expr = Node Evaluated NT.Expression
data instance Node Evaluated Expression where
  Var         :: String -> Node Evaluated Expression
  Hole        :: String -> Node Evaluated Expression
  Literal     :: Literal -> Node Evaluated Expression
  List        :: [AST Evaluated Expression] -> Node Evaluated Expression
  Tuple       :: [AST Evaluated Expression] -> Node Evaluated Expression
  Record      :: [(String,  AST Evaluated Expression)] -> Node Evaluated Expression
  Match       :: AST Evaluated Expression -> [AST Evaluated (NT.Case Expression)] -> Node Evaluated Expression
  Lambda      :: [String] ->   AST Evaluated Expression ->  Node Evaluated Expression
  Application ::  AST Evaluated Expression -> [AST Evaluated Expression] -> Node Evaluated Expression
  Block       :: [AST Evaluated Statement] -> Node Evaluated Expression
deriving instance Show (Node Evaluated NT.Expression)
deriving instance Show (AST Evaluated NT.Expression)


data instance Node Evaluated Statement where
  Return       :: AST Evaluated Expression ->  Node Evaluated Statement
  Procedure    :: AST Evaluated Expression ->  Node Evaluated Statement
  -- TODO:SUGGESTION turn declarations into Let bindings (Let .. in) expressions, which would require a new data constructor for expressions
  Declaration  :: Node Evaluated NT.Declaration ->  Node Evaluated Statement
deriving instance Show (Node Evaluated NT.Statement)
deriving instance Show (AST Evaluated NT.Statement)


data instance Node Evaluated NT.Declaration where
  Let  :: String -> AST Evaluated Expression    -> Node Evaluated NT.Declaration
  Type :: String -> AST Evaluated NT.Type -> Node Evaluated NT.Declaration
  Kind :: String -> AST Evaluated NT.Kind -> Node Evaluated NT.Declaration
deriving instance Show (Node Evaluated NT.Declaration)


data instance Node Evaluated (NT.Case Expression) where
  Case :: AST Evaluated (Pattern NT.Expression) -> AST Evaluated NT.Expression -> Node Evaluated (NT.Case NT.Expression)
deriving instance Show (Node Evaluated (NT.Case Expression))
deriving instance Show (AST Evaluated (NT.Case Expression))

data instance Node Evaluated (Pattern Expression) where
  Wildcard  :: Node Evaluated (Pattern Expression)
  Id        :: String -> Node Evaluated (Pattern Expression)
  PatHole   :: String -> Node Evaluated (Pattern Expression)
  PatLit    :: Literal  -> Node Evaluated (Pattern Expression)
  PatTuple  :: [AST Evaluated (Pattern Expression)] -> Maybe String  -> Node Evaluated (Pattern Expression)
  PatList   :: [AST Evaluated (Pattern Expression)] -> Maybe String  -> Node Evaluated (Pattern Expression)
  PatRecord :: [(String, Maybe § AST Evaluated (Pattern Expression))] -> Maybe String  -> Node Evaluated (Pattern Expression)
  PatData   :: String -> [AST Evaluated (Pattern Expression)]  -> Node Evaluated (Pattern Expression)
deriving instance Show (Node Evaluated (NT.Pattern Expression))
deriving instance Show (AST Evaluated (NT.Pattern Expression))
