module Saga.Parser.Monad where

import           Control.Monad.State        (StateT)
import           Control.Monad.Trans.Except (Except)
import qualified Saga.Lexer.Lexer           as L
import           Saga.Parser.Errors         (Error, Warning)





data ParserState = ParserState
  { errors   :: [Error]
  , warnings :: [Warning]
  }


newtype ParserM e s a = Parser (StateT s (Except e) a)


type Parser = ParserM Error ParserState


