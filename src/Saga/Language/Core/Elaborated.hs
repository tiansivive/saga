{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Core.Elaborated where

import           Saga.Language.Core.Literals

import qualified Saga.Language.Core.AST      as NodeType (NodeType (..))
import           Saga.Language.Core.AST

import           Saga.Utils.TypeLevel        (type (§))



data instance AST Elaborated e where
    Raw       :: Node Elaborated e -> AST Elaborated e
    Annotated :: Node Elaborated e -> Node Elaborated (Annotation e)  -> AST Elaborated e


data instance  Node Elaborated Expression where
  Var         :: String ->  Node Elaborated Expression
  Hole        :: String ->  Node Elaborated Expression

  Protocol    :: String ->  Node Elaborated Expression

  Literal     :: Literal ->  Node Elaborated Expression

  List        :: [AST Elaborated Expression] ->  Node Elaborated Expression
  Tuple       :: [AST Elaborated Expression] ->  Node Elaborated Expression
  Record      :: [(String,  AST Elaborated Expression)] ->  Node Elaborated Expression

  Match       ::  AST Elaborated Expression -> [AST Elaborated NodeType.Case] ->  Node Elaborated Expression
  Lambda      :: [String] ->   AST Elaborated Expression ->  Node Elaborated Expression
  Application :: AST Elaborated Expression -> [ AST Elaborated Expression] ->  Node Elaborated Expression

  Block       :: [AST Elaborated Statement] ->  Node Elaborated Expression

data instance Node Elaborated NodeType.Case where
  Case :: AST Elaborated Pattern -> AST Elaborated Expression -> Node Elaborated NodeType.Case

data instance Node Elaborated Pattern where
  Wildcard  :: Node Elaborated Pattern
  Id        :: String -> Node Elaborated Pattern
  PatHole   :: String -> Node Elaborated Pattern
  PatLit    :: Literal  -> Node Elaborated Pattern
  PatTuple  :: [AST Elaborated Pattern] -> Maybe String  -> Node Elaborated Pattern
  PatList   :: [AST Elaborated Pattern] -> Maybe String  -> Node Elaborated Pattern
  PatRecord :: [(String, Maybe § AST Elaborated Pattern)] -> Maybe String  -> Node Elaborated Pattern
  PatData   :: String -> [AST Elaborated Pattern]  -> Node Elaborated Pattern


data instance Node Elaborated Statement where
  Return       :: AST Elaborated Expression ->  Node Elaborated Statement
  Procedure    :: AST Elaborated Expression ->  Node Elaborated Statement
  Declaration  :: AST Elaborated NodeType.Declaration ->  Node Elaborated Statement

data instance Node Elaborated NodeType.Declaration where
  Let  :: String -> Node Elaborated Expression    -> Node Elaborated NodeType.Declaration
  Type :: String -> Node Elaborated NodeType.Type -> Node Elaborated NodeType.Declaration
  Kind :: String -> Node Elaborated NodeType.Kind -> Node Elaborated NodeType.Declaration
