{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Syntax.Desugared.Values where

import qualified Saga.Language.Syntax.AST             as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Desugared.AST
import           Saga.Language.Syntax.Desugared.Kinds
import           Saga.Language.Syntax.Desugared.Types

import           Saga.Language.Syntax.Literals
import           Saga.Utils.TypeLevel                 (type (§))

type Expr = Node Desugared Expression
data instance  Node Desugared Expression where
  Var         :: String -> Node Desugared Expression
  Hole        :: String -> Node Desugared Expression
  Literal     :: Literal -> Node Desugared Expression
  List        :: [AST Desugared Expression] -> Node Desugared Expression
  Tuple       :: [AST Desugared Expression] -> Node Desugared Expression
  Record      :: [(String,  AST Desugared Expression)] -> Node Desugared Expression
  Match       :: AST Desugared Expression -> [AST Desugared (NT.Case Expression)] -> Node Desugared Expression
  Lambda      :: [String] ->   AST Desugared Expression ->  Node Desugared Expression
  Application ::  AST Desugared Expression -> [AST Desugared Expression] -> Node Desugared Expression
  Block       :: [AST Desugared Statement] -> Node Desugared Expression
deriving instance Show (Node Desugared NT.Expression)
deriving instance Show (AST Desugared NT.Expression)


data instance Node Desugared Statement where
  Return       :: AST Desugared Expression ->  Node Desugared Statement
  Procedure    :: AST Desugared Expression ->  Node Desugared Statement
  Declaration  :: AST Desugared NT.Declaration ->  Node Desugared Statement
deriving instance Show (Node Desugared NT.Statement)
deriving instance Show (AST Desugared NT.Statement)


data instance Node Desugared NT.Declaration where
  Let  :: String -> Node Desugared Expression    -> Node Desugared NT.Declaration
  Type :: String -> Node Desugared NT.Type -> Node Desugared NT.Declaration
  Kind :: String -> Node Desugared NT.Kind -> Node Desugared NT.Declaration
deriving instance Show (Node Desugared NT.Declaration)
deriving instance Show (AST Desugared NT.Declaration)


data instance Node Desugared (NT.Case Expression) where
  Case :: AST Desugared (Pattern NT.Expression) -> AST Desugared (NT.Case Expression) -> Node Desugared (NT.Case NT.Expression)
deriving instance Show (Node Desugared (NT.Case Expression))
deriving instance Show (AST Desugared (NT.Case Expression))

data instance Node Desugared (Pattern Expression) where
  Wildcard  :: Node Desugared (Pattern Expression)
  Id        :: String -> Node Desugared (Pattern Expression)
  PatHole   :: String -> Node Desugared (Pattern Expression)
  PatLit    :: Literal  -> Node Desugared (Pattern Expression)
  PatTuple  :: [AST Desugared (Pattern Expression)] -> Maybe String  -> Node Desugared (Pattern Expression)
  PatList   :: [AST Desugared (Pattern Expression)] -> Maybe String  -> Node Desugared (Pattern Expression)
  PatRecord :: [(String, Maybe § AST Desugared (Pattern Expression))] -> Maybe String  -> Node Desugared (Pattern Expression)
  PatData   :: String -> [AST Desugared (Pattern Expression)]  -> Node Desugared (Pattern Expression)
deriving instance Show (Node Desugared (NT.Pattern Expression))
deriving instance Show (AST Desugared (NT.Pattern Expression))
