module Saga.Lexer.Tokens where 


import Data.ByteString.Lazy.Char8 (ByteString)

-- Lexer


data Token
  = Id ByteString

  --  Keywords
  | Let
  | In
  | As
  | Where
  | With
  | If
  | Then
  | Else
  | Match

  -- Modules
  | Module
  | Import
  | Qualified
  | Hiding
  | Hide

  -- Data structures
  | Data
  | Derive

  -- Types
  | Type
  | Kind

  -- Interfaces
  | Interface
  | Protocol
  | Instance

  -- reserved symbols
  | Arrow
  | Colon
  | Comma
  | Equals
  | Pipe
  | Dot
  | BackSlash
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LCurly
  | RCurly

  -- primitives
  | Number Int
  | String ByteString
  | Boolean Bool

  | Newline
  | EOF
  deriving (Show, Eq)