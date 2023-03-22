module Saga.Lexer.Tokens where 


import Data.ByteString.Lazy.Char8 (ByteString)

-- Lexer


data Token
  = Id ByteString
  --  Keywords
  | Let
  | In
  | Where
  | With
  | If
  | Then
  | Else
  | Match

  -- reserved symbols
  | Arrow
  | Colon
  | Comma
  | Equals
  | Pipe
  | Dot
  | LParen
  | RParen
  | LBrack
  | RBrack

  -- primitives
  | Number Int
  | String ByteString
  | Boolean Bool

  | EOF
  deriving (Show, Eq)