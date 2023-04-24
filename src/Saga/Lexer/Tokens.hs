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
  | Alias
  | Kind
  | Forall
  | Exists
  | Proof
  | Infer


  -- Interfaces
  | Interface
  | Protocol
  | Instance
  | Implements

  -- reserved symbols
  | Arrow
  | BackArrow
  | FatArrow
  | PipeArrow
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
