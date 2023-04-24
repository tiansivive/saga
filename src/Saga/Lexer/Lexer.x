{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Saga.Lexer.Lexer where
-- Using BS as they're more efficient for input streams
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Saga.Lexer.Tokens 


}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit      = [0-9]
$alpha      = [a-zA-Z]
$nl         = [\n]
$backslash  = [\\]
$ws         = [[\ \t\f\v\r]] -- whitespace char set without newline

@id   = ($alpha | \_) ($alpha | $digit | \_ | \' | \? )*

tokens :-


    <0> $white+                    { skip }
    --<0> $nl+                    { tok Newline }

    <0> module                            { tok Module }
    <0> import                            { tok Import } 


    <0> let                  { tok Let }
    <0> with                 { tok With }
    <0> where                { tok Where }
    <0> in                   { tok In }
    <0> as                   { tok As }
    <0> if                   { tok If }
    <0> then                 { tok Then }
    <0> else                 { tok Else }
    <0> unless               { tok Unless }
    <0> match                { tok Match }
    <0> return               { tok Return }
    
    <0> data                 { tok Data }

 

    <0> $digit+             { tokNumber }
    <0> (\"[^\"]*\")        { tokString }
    <0> yes | on  | true    { tokBoolean True }
    <0> no  | off | false   { tokBoolean False }
    <0> @id                 { tokId }
    
    <0> "("                 { tok LParen }
    <0> ")"                 { tok RParen }
    <0> "["                 { tok LBrack }
    <0> "]"                 { tok RBrack }
    <0> "{"                 { tok LCurly }
    <0> "}"                 { tok RCurly }

    <0> ":"                 { tok Colon }
    <0> ","                 { tok Comma }
    <0> "->"                { tok Arrow }
    <0> "<-"                { tok BackArrow }
    <0> "="                 { tok Equals }
    <0> "|"                 { tok Pipe }
    <0> "."                 { tok Dot }
    <0> $backslash          { tok BackSlash }

    <0> "+"                 { tok $ Operator "+" }
    <0> "-"                 { tok $ Operator "-" }
    <0> "*"                 { tok $ Operator "*" }
    <0> "/"                 { tok $ Operator "/" }
    <0> "%"                 { tok $ Operator "%" }
    <0> "^"                 { tok $ Operator "^" }

    <0> "<"                 { tok $ Operator "<" }
    <0> "<="                { tok $ Operator "<=" }
    <0> ">"                 { tok $ Operator ">" }
    <0> ">="                { tok $ Operator ">=" }

    <0> "||"                { tok $ Operator "||" }
    <0> "&&"                { tok $ Operator "&&" }
    <0> "!"                 { tok $ Operator "!" }
    
    <0> "=="                { tok $ Operator "==" }
    <0> "!="                { tok $ Operator "!=" }

    <0> ">>"                { tok $ Operator ">>" }
    <0> "<<"                { tok $ Operator "<<" }
    
    <0> "|>"                { tok $ Operator "|>" }
    <0> "<|"                { tok $ Operator "<|" }

    <0> "$"                 { tok $ Operator "$" }
    <0> "#"                 { tok $ Operator "#" }
    <0> "@"                 { tok $ Operator "#" }

    




    -- comments
    <0>       "/*" { nestComment `andBegin` comment }
    <0>       "*/" { \_ _ -> alexError "Error: unexpected closing comment" }
    <comment> "/*" { nestComment }
    <comment> "*/" { unnestComment }
    <comment> .    ;
    <comment> $nl  ;


{

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } 
  | NoInfo 
  
data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  }

-- | faking Eq isntances so we dont get problems with the Expr and Type Eq 
-- | TODO: this definitely needs to change!!!!!
instance Eq Range where
  r1 == r2 = True

instance Eq RangedToken where 
  rt1 == rt2 = True

instance Show Range where
  show _ = ""
instance Show RangedToken where
  show t =  show $ rtToken t

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
    { rtToken = String $ BS.tail $ BS.take (len -1) str
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
