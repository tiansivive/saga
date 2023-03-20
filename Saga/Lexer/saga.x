{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
{-# LANGUAGE OverloadedStrings #-}

module Saga.Lexer.Lexer where
-- Using BS as they're more efficient for input streams
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS


}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

    <0> $white+             { skip }

    <0> let                 { tok Let }
    <0> in                  { tok In }
    <0> where               { tok Where }
    <0> with                { tok With }
    <0> if                  { tok If }
    <0> then                { tok Then }
    <0> else                { tok Else }
    <0> match               { tok Match }



    <0> $digit+             { tokNumber }
    <0> (\"[^\"]*\")        { tokString }
    <0> yes | on  | true    { tokBoolean True }
    <0> no  | off | false   { tokBoolean False }
    <0> @id                 { tokId }
    
    <0> "("                 { tok LParen }
    <0> ")"                 { tok RParen }
    <0> "["                 { tok LBrack }
    <0> "]"                 { tok RBrack }

    <0> ":"                 { tok Colon }
    <0> ","                 { tok Comma }
    <0> "->"                { tok Arrow }
    <0> "="                 { tok Equals }
    <0> "|"                 { tok Pipe }
    <0> "."                 { tok Dot }

    -- comments
    <0>       "/*" { nestComment `andBegin` comment }
    <0>       "*/" { \_ _ -> alexError "Error: unexpected closing comment" }
    <comment> "/*" { nestComment }
    <comment> "*/" { unnestComment }
    <comment> .    ;
    <comment> \n   ;





{
-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { nestLevel = 0
    }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)


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

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Id $ BS.take len str
    , rtRange = mkRange inp len
    }

tok :: Token -> AlexAction RangedToken
tok t inp len =
  pure RangedToken
    { rtToken = t
    , rtRange = mkRange inp len
    }

tokNumber :: AlexAction RangedToken
tokNumber inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Number $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokString :: AlexAction RangedToken
tokString inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = String $ BS.take len str
    , rtRange = mkRange inp len
    }

tokBoolean :: Bool -> AlexAction RangedToken
tokBoolean bool inp len =
  pure RangedToken
    { rtToken = Boolean bool
    , rtRange = mkRange inp len
    }


nestComment, unnestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

scanMany :: String -> Either String [RangedToken]
scanMany input = runAlex (BS.pack input) go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go


}
