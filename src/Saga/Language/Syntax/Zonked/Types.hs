{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Zonked.Types where

import           Data.Map                            (Map)
import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Literals

import           Saga.Language.Syntax.Zonked.Kinds
import           Saga.Language.Typechecker.Variables (Variable)

import           Saga.Utils.TypeLevel                (type (§))

type Type = Node Zonked NT.Type
data instance Node Zonked NT.Type where
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
deriving instance Show (Variable Type)




