{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Zonked.Values where




import qualified Saga.Language.Syntax.AST          as NT (NodeType (..))
import           Saga.Language.Syntax.AST


import           Saga.Language.Syntax.Zonked.AST
import           Saga.Language.Syntax.Zonked.Kinds
import           Saga.Language.Syntax.Zonked.Types

import           Saga.Language.Syntax.Literals
import           Saga.Utils.TypeLevel              (type (§))


data instance Node Zonked Expression where
  Var         :: (Variable § Node Zonked Expression) -> Node Zonked Expression
  Hole        :: String -> Node Zonked Expression
  Literal     :: Literal -> Node Zonked Expression
  List        :: [AST Zonked Expression] -> Node Zonked Expression
  Tuple       :: [AST Zonked Expression] -> Node Zonked Expression
  Record      :: [(String,  AST Zonked Expression)] -> Node Zonked Expression
  Match       :: AST Zonked Expression -> [AST Zonked (NT.Case Expression)] -> Node Zonked Expression
  Lambda      :: [String] ->   AST Zonked Expression ->  Node Zonked Expression
  Application ::  AST Zonked Expression -> [AST Zonked Expression] -> Node Zonked Expression
  Block       :: [AST Zonked Statement] -> Node Zonked Expression
deriving instance Show (Node Zonked NT.Expression)
deriving instance Show (AST Zonked NT.Expression)


data instance Node Zonked Statement where
  Return       :: AST Zonked Expression ->  Node Zonked Statement
  Procedure    :: AST Zonked Expression ->  Node Zonked Statement
  Declaration  :: Node Zonked NT.Declaration ->  Node Zonked Statement
deriving instance Show (Node Zonked NT.Statement)
deriving instance Show (AST Zonked NT.Statement)


data instance Node Zonked NT.Declaration where
  Let  :: String -> AST Zonked Expression    -> Node Zonked NT.Declaration
  Type :: String -> AST Zonked NT.Type -> Node Zonked NT.Declaration
  Kind :: String -> AST Zonked NT.Kind -> Node Zonked NT.Declaration
deriving instance Show (Node Zonked NT.Declaration)


data instance Node Zonked (NT.Case Expression) where
  Case :: AST Zonked (Pattern NT.Expression) -> AST Zonked (NT.Case Expression) -> Node Zonked (NT.Case NT.Expression)
deriving instance Show (Node Zonked (NT.Case Expression))
deriving instance Show (AST Zonked (NT.Case Expression))

data instance Node Zonked (Pattern Expression) where
  Wildcard  :: Node Zonked (Pattern Expression)
  Id        :: String -> Node Zonked (Pattern Expression)
  PatHole   :: String -> Node Zonked (Pattern Expression)
  PatLit    :: Literal  -> Node Zonked (Pattern Expression)
  PatTuple  :: [AST Zonked (Pattern Expression)] -> Maybe String  -> Node Zonked (Pattern Expression)
  PatList   :: [AST Zonked (Pattern Expression)] -> Maybe String  -> Node Zonked (Pattern Expression)
  PatRecord :: [(String, Maybe § AST Zonked (Pattern Expression))] -> Maybe String  -> Node Zonked (Pattern Expression)
  PatData   :: String -> [AST Zonked (Pattern Expression)]  -> Node Zonked (Pattern Expression)
deriving instance Show (Node Zonked (NT.Pattern Expression))
deriving instance Show (AST Zonked (NT.Pattern Expression))


data instance Variable (Node Zonked Expression) where
  Identifier    :: String -> Variable § Node Zonked Expression
deriving instance Show (Variable § Node Zonked Expression)
