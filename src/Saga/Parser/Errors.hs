module Saga.Parser.Errors where

import qualified Saga.Lexer.Lexer   as L
import           Saga.Parser.Layout (LayoutStack)

data Info a = Info
  { range   :: L.Range
  , tokens  :: [L.RangedToken]
  , stack   :: LayoutStack
  , errType :: a
  } deriving (Show, Eq)


data ErrorType = D
data WarningType = W


type Error = Info ErrorType
type Warning = Info WarningType
