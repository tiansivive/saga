{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.Types where

import           Data.Map                              (Map)
import qualified Saga.Language.Syntax.AST              as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Elaborated.Kinds
import           Saga.Language.Typechecker.Variables   (Variable)

import           Saga.Utils.TypeLevel                  (type (§))

type Type = Node Elaborated NT.Type
data instance Node Elaborated NT.Type where
    Singleton   :: Literal -> Type
    Tuple       :: [Type] -> Type
    Record      :: [(String, Type)] -> Type
    Union       :: [Type] -> Type
    Arrow       :: Type -> Type -> Type
    Data        :: String -> Kind -> Type
    Applied     :: Type -> Type -> Type
    Var         :: Variable Type -> Type
    Void        :: Type
    Any         :: Type
deriving instance Show Type

data instance Variable Type where
  Poly              :: String -> Kind -> Variable Type
  Existential       :: String -> Kind -> Variable Type
  Local             :: String -> Kind -> Variable Type
  Skolem            :: String -> Kind -> Variable Type
  Rigid             :: String -> Kind -> Variable Type
  Scoped            :: String -> Kind -> Variable Type
  Unification       :: String -> Kind -> Variable Type
  Instantiation     :: String -> Kind -> Variable Type
deriving instance Show (Variable Type)




