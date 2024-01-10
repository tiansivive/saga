{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}

{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Core.Evaluated where

import           Saga.Language.Core.Literals

import qualified Saga.Language.Core.AST      as NodeType (NodeType (..))
import           Saga.Language.Core.AST

import           Saga.Utils.TypeLevel        (type (§))



data instance AST Evaluated e where
    Raw       :: Node Evaluated e -> AST Evaluated e
    Annotated :: Node Evaluated e -> Node Evaluated (Annotation e)  -> AST Evaluated e


data instance  Node Evaluated Expression where
  Var         :: String ->  Node Evaluated Expression
  Hole        :: String ->  Node Evaluated Expression

  Literal     :: Literal ->  Node Evaluated Expression

  List        :: [AST Evaluated Expression] ->  Node Evaluated Expression
  Tuple       :: [AST Evaluated Expression] ->  Node Evaluated Expression
  Record      :: [(String,  AST Evaluated Expression)] ->  Node Evaluated Expression

  Match       ::  AST Evaluated Expression -> [AST Evaluated NodeType.Case] ->  Node Evaluated Expression
  Lambda      :: [String] ->   AST Evaluated Expression ->  Node Evaluated Expression
  Application ::  AST Evaluated Expression -> [ AST Evaluated Expression] ->  Node Evaluated Expression

  Block       :: [AST Evaluated Statement] ->  Node Evaluated Expression


data instance Node Evaluated Statement where
  Return       :: AST Evaluated Expression ->  Node Evaluated Statement
  Procedure    :: AST Evaluated Expression ->  Node Evaluated Statement
  Declaration  :: AST Evaluated NodeType.Declaration ->  Node Evaluated Statement
data instance Node Evaluated NodeType.Case where
  Case :: AST Evaluated Pattern -> AST Evaluated Expression -> Node Evaluated NodeType.Case

data instance Node Evaluated Pattern where
  Wildcard  :: Node Evaluated Pattern
  Id        :: String -> Node Evaluated Pattern
  PatHole   :: String -> Node Evaluated Pattern
  PatLit    :: Literal  -> Node Evaluated Pattern
  PatTuple  :: [AST Evaluated Pattern] -> Maybe String  -> Node Evaluated Pattern
  PatList   :: [AST Evaluated Pattern] -> Maybe String  -> Node Evaluated Pattern
  PatRecord :: [(String, Maybe § AST Evaluated Pattern)] -> Maybe String  -> Node Evaluated Pattern
  PatData   :: String -> [AST Evaluated Pattern]  -> Node Evaluated Pattern

data instance Node Evaluated NodeType.Declaration where
  Let  :: String -> Node Evaluated Expression    -> Node Evaluated NodeType.Declaration
  Type :: String -> Node Evaluated NodeType.Type -> Node Evaluated NodeType.Declaration
  Kind :: String -> Node Evaluated NodeType.Kind -> Node Evaluated NodeType.Declaration
