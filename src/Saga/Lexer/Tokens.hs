module Saga.Lexer.Tokens where

import           Data.ByteString.Lazy.Char8 (ByteString)

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
  | Unless
  | Match
  | Return

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
  | BackArrow
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
  | Operator ByteString

  -- primitives
  | Number Int
  | String ByteString
  | Boolean Bool

  | Newline
  | EOF
  deriving (Show, Eq)
