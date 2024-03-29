{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Saga.Lexer.Lexer where
-- Using BS as they're more efficient for input streams
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Saga.Lexer.Tokens 
import Debug.Trace

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

@ident = (\n) (\n | $ws)* $ws*

tokens :-


    $ws+          { skip }
  
    module               { tok Module }
    import               { tok Import } 


    let                  { tok Let }
    with                 { tok With }
    where                { tok Where }
    in                   { tok In }
    as                   { tok As }
    if                   { tok If }
    then                 { tok Then }
    else                 { tok Else }
    unless               { tok Unless }
    match                { tok Match }
    <block> return       { tok Return }
    
    data                 { tok Data }

    type                 { tok Type }
    forall               { tok Forall }

    exists               { tok Exists }

    alias                { tok Alias }
    protocol             { tok Protocol }
    implements           { tok Implements }
    implementation       { tok Implements }
    impl                 { tok Instance }
    instance             { tok Instance }

 

    $digit+             { mkTok (Number . read . BS.unpack) }
    (\"[^\"]*\")        { mkTok (String . trimQuotes) }
    yes | on  | true    { mkTok $ const $ Boolean True }
    no  | off | false   { mkTok $ const $ Boolean False }
    @id                 { mkTok Id  }
    @hole               { mkTok Hole }
    
    "("                 { tok LParen }
    ")"                 { tok RParen }
    "["                 { tok LBrack }
    "]"                 { tok RBrack }
    

    ":"                 { tok Colon }
    ";"                 { tok SemiColon }
    ","                 { tok Comma }
    "->"                { tok Arrow }
    <block> "<-"        { tok BackArrow }
    "=>"                { tok FatArrow }
    "|->"               { tok PipeArrow }
    "="                 { tok Equals }
    "|"                 { tok Pipe }
    "."                 { tok Dot }
    "`"                 { tok Backtick }
    "::"                { tok Section }
    $backslash          { tok BackSlash }

    "+"                 { tok $ Operator "+" }
    "-"                 { tok $ Operator "-" }
    "*"                 { tok $ Operator "*" }
    "/"                 { tok $ Operator "/" }
    "%"                 { tok $ Operator "%" }
    "^"                 { tok $ Operator "^" }

    "++"                { tok $ Operator "++" }

    "<"                 { tok $ Operator "<" }
    "<="                { tok $ Operator "<=" }
    ">"                 { tok $ Operator ">" }
    ">="                { tok $ Operator ">=" }

    "||"                { tok $ Operator "||" }
    "&&"                { tok $ Operator "&&" }
    "!"                 { tok $ Operator "!" }
    
    "=="                { tok $ Operator "==" }
    "!="                { tok $ Operator "!=" }

    ">>"                { tok $ Operator ">>" }
    "<<"                { tok $ Operator "<<" }
    
    "|>"                { tok $ Operator "|>" }
    "<|"                { tok $ Operator "<|" }


    "$"                 { tok $ Operator "$" }
    "#"                 { tok $ Operator "#" }
    "@"                 { tok $ Operator "#" }

    <0, block> "{"                 { handleCurlyBrace }
    <0, block> "}"                 { handleCloseCurlyBrace }
 

    <0, block> @ident          { identation } 
    -- <block> .               { skip } 
 

    -- comments
    <0, block> "/*" { nestComment `andBegin` comment }
    <0, block> "*/" { \_ _ -> alexError "Error: unexpected closing comment" }
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
  { commentLevel  :: Int
  , identLevel :: Int
  , indentStateStack :: [Int]
  , lastToken :: Maybe Token
  } deriving (Show)




alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { commentLevel = 0
    , identLevel = 0
    , indentStateStack = [0]
    , lastToken = Nothing
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



tok :: Token -> AlexAction RangedToken
tok = mkTok . const

mkTok :: (ByteString -> Token) -> AlexAction RangedToken
mkTok cons inp@(_, _, str, _) len = do
  current <- alexGetStartCode
  if current == comment then 
    skip inp len
  else do
    let tok = cons $ BS.take len str
    modify $ \s -> s{lastToken = Just tok }
    pure RangedToken
      { rtToken = tok
      , rtRange = mkRange inp len
      }

trimQuotes :: ByteString -> ByteString
trimQuotes str = BS.tail $ BS.take (BS.length str -1) str

handleCurlyBrace :: AlexAction RangedToken
handleCurlyBrace inp len = do
  state <- get 
  -- traceM $ "\nOpening in state: " ++ show state
  case lastToken state of
    Just Arrow -> do
      modify $ \s -> s{indentStateStack = block : indentStateStack s}
      alexSetStartCode block
    _ -> modify $ \s -> s{indentStateStack = 0 : indentStateStack s}
  tok LCurly inp len


handleCloseCurlyBrace ::  AlexAction RangedToken
handleCloseCurlyBrace inp len = do
  state <- get 
  let stack = indentStateStack state
  -- traceM $ "\nClosing in state: " ++ show state
  if length stack == 1 then
    alexError "Error: unexpected closing }"
  else do
    let stack' = tail stack
    modify $ \s -> s{indentStateStack = stack' }
    alexSetStartCode $ head stack'
    tok RCurly inp len


identation :: AlexAction RangedToken
identation input@(_, _, str, _) len = do
  state <- get
  let stack = indentStateStack state
  let len' = fromIntegral $ BS.length $ pruneIdentation len str
  let stateIndent = identLevel state
  -- traceM "\nIndentation"
  -- traceM $ "State: " ++ show state
  -- traceM $ "Current indent len: " ++ show len'
  -- traceM $ "Current input: " ++ show (BS.take len str)

  
  if len' > stateIndent then do 
    -- traceM "len > stateIndent "
    case lastToken state of
      Just LCurly -> modify $ \s -> s{identLevel = len'}
      _           -> do
        modify $ \s -> s{indentStateStack = 0 : stack, identLevel = len'}
        alexSetStartCode 0
    skip input len
  else if stateIndent == 0 then do
    -- traceM "state ident == 0"
    modify $ \s -> s{identLevel = len'}
    skip input len
  else if len' < stateIndent then do 
    -- traceM "len < stateIndent "
    let previousCode = head stack
    if previousCode == block then do -- block state popping is handled by curly braces handling
      modify $ \s -> s{ identLevel = len' }
      skip input len
    else do
      let stack' = tail stack
      let nextCode = head stack'
      modify $ \s -> s{ indentStateStack = stack', identLevel = len' }
      alexSetStartCode nextCode
      if nextCode == block then
        tok SemiColon input len
      else do
        skip input len
  else if head stack == block then do
    -- traceM "code == block"
    tok SemiColon input len
  else skip input len
              

pruneIdentation :: Int64 -> ByteString -> ByteString 
pruneIdentation len str = BS.takeWhileEnd ((/=) '\n') $ BS.take len str

nestComment, unnestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{commentLevel = commentLevel s + 1}
  state <- get
  skip input len
unnestComment input len = do
  state <- get
  let level = commentLevel state - 1
  put state{commentLevel = level}
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
