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
-- $forall     = [\∀]
-- $exists     = [\∃]
$ws         = [[\ \t\f\v\r]] -- whitespace char set without newline

@id   = ($alpha | \_) ($alpha | $digit | \_ | \' | \? )*
@hole = (\?) ($alpha | \_) ($alpha | $digit | \_ | \' | \? )*

@ident = (\n)+ $ws*

tokens :-


    <0> $white+                     { skip }


    <0, block> module               { tok Module }
    <0, block> import               { tok Import } 


    <0, block> let                  { tok Let }
    <0, block> with                 { tok With }
    <0, block> where                { tok Where }
    <0, block> in                   { tok In }
    <0, block> as                   { tok As }
    <0, block> if                   { tok If }
    <0, block> then                 { tok Then }
    <0, block> else                 { tok Else }
    <0, block> unless               { tok Unless }
    <0, block> match                { tok Match }
    <block> return                  { tok Return }
    
    <0, block> data                 { tok Data }

    <0, block> type                 { tok Type }
    <0, block> forall               { tok Forall }

    <0, block> exists               { tok Exists }

    <0, block> alias                { tok Alias }
    <0, block> protocol             { tok Protocol }
    <0, block> implements           { tok Implements }
    <0, block> implementation       { tok Implements }
    <0, block> impl                 { tok Instance }
    <0, block> instance             { tok Instance }

 

    <0, block> $digit+             { tokNumber }
    <0, block> (\"[^\"]*\")        { tokString }
    <0, block> yes | on  | true    { tokBoolean True }
    <0, block> no  | off | false   { tokBoolean False }
    <0, block> @id                 { tokId }
    <0, block> @hole               { tokHole }
    
    <0, block> "("                 { tok LParen }
    <0, block> ")"                 { tok RParen }
    <0, block> "["                 { tok LBrack }
    <0, block> "]"                 { tok RBrack }
    

    <0, block> ":"                 { tok Colon }
    <0, block> ";"                 { tok SemiColon }
    <0, block> ","                 { tok Comma }
    <0, block> "->"                { tok Arrow }
    <block> "<-"                   { tok BackArrow }
    <0, block> "=>"                { tok FatArrow }
    <0, block> "|->"               { tok PipeArrow }
    <0, block> "="                 { tok Equals }
    <0, block> "|"                 { tok Pipe }
    <0, block> "."                 { tok Dot }
    <0, block> "`"                 { tok Backtick }
    <0, block> "::"                { tok Section }
    <0, block> $backslash          { tok BackSlash }

    <0, block> "+"                 { tok $ Operator "+" }
    <0, block> "-"                 { tok $ Operator "-" }
    <0, block> "*"                 { tok $ Operator "*" }
    <0, block> "/"                 { tok $ Operator "/" }
    <0, block> "%"                 { tok $ Operator "%" }
    <0, block> "^"                 { tok $ Operator "^" }

    <0, block> "++"                { tok $ Operator "++" }

    <0, block> "<"                 { tok $ Operator "<" }
    <0, block> "<="                { tok $ Operator "<=" }
    <0, block> ">"                 { tok $ Operator ">" }
    <0, block> ">="                { tok $ Operator ">=" }

    <0, block> "||"                { tok $ Operator "||" }
    <0, block> "&&"                { tok $ Operator "&&" }
    <0, block> "!"                 { tok $ Operator "!" }
    
    <0, block> "=="                { tok $ Operator "==" }
    <0, block> "!="                { tok $ Operator "!=" }

    <0, block> ">>"                { tok $ Operator ">>" }
    <0, block> "<<"                { tok $ Operator "<<" }
    
    <0, block> "|>"                { tok $ Operator "|>" }
    <0, block> "<|"                { tok $ Operator "<|" }


    <0, block> "$"                 { tok $ Operator "$" }
    <0, block> "#"                 { tok $ Operator "#" }
    <0, block> "@"                 { tok $ Operator "#" }

    <0> "{"                 { nestBlock `andBegin` block }
    <0> "}"                 { \_ _ -> alexError "Error: unexpected closing }" }
    
    <block> "{"             { nestBlock }
    <block> "}"             { unnestBlock }

    <block> @ident          { identation } 
    <block> .               { skip } 
 

    -- comments
    <0, block>  "/*" { nestComment `andBegin` comment }
    <0, block>  "*/" { \_ _ -> alexError "Error: unexpected closing comment" }
    <comment>   "/*" { nestComment }
    <comment>   "*/" { unnestComment }
    <comment>   .    ;
    <comment>   $nl  ;


{

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } 

  
data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  }

instance Eq Range where
  r1 == r2 = start r1 == start r2 && stop r1 == stop r2

instance Eq RangedToken where 
  rt1 == rt2 = rtToken rt1 == rtToken rt2 && rtRange rt1 == rtRange rt2

instance Show Range where
  show (Range start stop) = "| " ++ show start ++ " <-> " ++ show stop ++ " |"
instance Show RangedToken where
  show t = show $ rtToken t

-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  { nestLevel     :: Int
  , blockLevel :: Int
  , identLevel :: Int
  } deriving (Show)




alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { nestLevel = 0
    , blockLevel = 0
    , identLevel = 0
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

tokHole :: AlexAction RangedToken
tokHole inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Hole $ BS.take len str
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


nestBlock :: AlexAction RangedToken
nestBlock inp len = do
  modify $ \s -> s{blockLevel = blockLevel s + 1}
  tok LCurly inp len
unnestBlock :: AlexAction RangedToken
unnestBlock inp len = do
  state <- get
  let level = blockLevel state - 1
  put state{blockLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  tok RCurly inp len


identation :: AlexAction RangedToken
identation input len = do
  state <- get
  let len' = fromIntegral len
  let currentIndent = identLevel state
  if len' > currentIndent
    then do 
      modify $ \s -> s{identLevel = len'}
      skip input len
    else if len' < currentIndent
    then do 
      modify $ \s -> s{identLevel = identLevel s - len'}
      tok SemiColon input len
    else tok SemiColon input len
              


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
