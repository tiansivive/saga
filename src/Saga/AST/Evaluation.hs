
module Saga.AST.Evaluation where

import Saga.AST.Syntax (Expr(..), Literal(..), Name(..), Definition(..))

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

data Value a
  = VInt Int
  | VBool Bool
  | VString String
  | VList [Value a]
  | VTuple [Value a]
  | VRecord [(String, Value a)]
  | VClosure [Name a] (Expr a) 
  deriving (Show)


eval :: Expr a -> Value a
eval (Declaration (Def _ (Name _ name) e)) = eval e
eval (Lit l) = evalLit l
eval (Lambda _ args body) = VClosure args body
eval (Block _ defs e) = eval e
eval (Identifier (Name _ name)) = VString $ BS.unpack name 

evalLit :: Literal a -> Value a
evalLit (LInt _ int) = VInt int
evalLit (LBool _ bool) = VBool bool
evalLit (LString _ str) = VString $ BS.unpack str
evalLit (LList _ list) = VList $ eval <$> list
evalLit (LTuple _ tuple) = VTuple $ eval <$> tuple
evalLit (LRecord _ record) = VRecord $ evalPair <$> record
  where evalPair ((Name _ name), e) = (BS.unpack name, eval e)


id = { fn: \n -> 1, foo: yes, num: 42, str: "String", list: [1,2,3,4], tuple: (on, off, 1, 2) }