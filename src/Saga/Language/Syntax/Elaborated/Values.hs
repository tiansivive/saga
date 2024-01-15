{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.Values where




import qualified Saga.Language.Syntax.AST              as NT (NodeType (..))
import           Saga.Language.Syntax.AST

import           Saga.Language.Syntax.Elaborated.AST
import           Saga.Language.Syntax.Elaborated.Kinds
import           Saga.Language.Syntax.Elaborated.Types

import           Saga.Language.Syntax.Literals
import           Saga.Utils.TypeLevel                  (type (§))


data instance Node Elaborated Expression where
  Var         :: (Variable § Node Elaborated Expression) -> Node Elaborated Expression
  Hole        :: String -> Node Elaborated Expression
  Literal     :: Literal -> Node Elaborated Expression
  List        :: [AST Elaborated Expression] -> Node Elaborated Expression
  Tuple       :: [AST Elaborated Expression] -> Node Elaborated Expression
  Record      :: [(String,  AST Elaborated Expression)] -> Node Elaborated Expression
  Match       :: AST Elaborated Expression -> [AST Elaborated (NT.Case Expression)] -> Node Elaborated Expression
  Lambda      :: [String] ->   AST Elaborated Expression ->  Node Elaborated Expression
  Application ::  AST Elaborated Expression -> [AST Elaborated Expression] -> Node Elaborated Expression
  Block       :: [AST Elaborated Statement] -> Node Elaborated Expression

  FieldAccess :: AST Elaborated Expression -> String -> Node Elaborated Expression
deriving instance Show (Node Elaborated NT.Expression)
deriving instance Show (AST Elaborated NT.Expression)


data instance Node Elaborated Statement where
  Return       :: AST Elaborated Expression ->  Node Elaborated Statement
  Procedure    :: AST Elaborated Expression ->  Node Elaborated Statement
  Declaration  :: Node Elaborated NT.Declaration ->  Node Elaborated Statement
deriving instance Show (Node Elaborated NT.Statement)
deriving instance Show (AST Elaborated NT.Statement)


data instance Node Elaborated NT.Declaration where
  Let  :: String -> AST Elaborated Expression    -> Node Elaborated NT.Declaration
  Type :: String -> AST Elaborated NT.Type -> Node Elaborated NT.Declaration
  Kind :: String -> AST Elaborated NT.Kind -> Node Elaborated NT.Declaration
deriving instance Show (Node Elaborated NT.Declaration)


data instance Node Elaborated (NT.Case Expression) where
  Case :: AST Elaborated (Pattern NT.Expression) -> AST Elaborated (NT.Case Expression) -> Node Elaborated (NT.Case NT.Expression)
deriving instance Show (Node Elaborated (NT.Case Expression))
deriving instance Show (AST Elaborated (NT.Case Expression))

data instance Node Elaborated (Pattern Expression) where
  Wildcard  :: Node Elaborated (Pattern Expression)
  Id        :: String -> Node Elaborated (Pattern Expression)
  PatHole   :: String -> Node Elaborated (Pattern Expression)
  PatLit    :: Literal  -> Node Elaborated (Pattern Expression)
  PatTuple  :: [AST Elaborated (Pattern Expression)] -> Maybe String  -> Node Elaborated (Pattern Expression)
  PatList   :: [AST Elaborated (Pattern Expression)] -> Maybe String  -> Node Elaborated (Pattern Expression)
  PatRecord :: [(String, AST Elaborated (Pattern Expression))] -> Maybe String  -> Node Elaborated (Pattern Expression)
  PatData   :: String -> [AST Elaborated (Pattern Expression)]  -> Node Elaborated (Pattern Expression)
deriving instance Show (Node Elaborated (NT.Pattern Expression))
deriving instance Show (AST Elaborated (NT.Pattern Expression))


data instance Variable (Node Elaborated Expression) where
    Identifier    :: String -> Variable § Node Elaborated Expression
    Evidence      :: String -> Variable § Node Elaborated Expression
deriving instance Show (Variable § Node Elaborated Expression)
