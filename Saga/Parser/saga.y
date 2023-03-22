{
module Saga.Parser.Parser where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.AST.Syntax as AST

}

%name parseSaga expr
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken T.EOF _ }
%token
  -- Identifiers
  id { L.RangedToken (T.Id _) _ }
  -- Constants
  number     { L.RangedToken (T.Number _) _ }
  string     { L.RangedToken (T.String _) _ }
  boolean    { L.RangedToken (T.Boolean _) _ }

  -- Keywords
  let        { L.RangedToken T.Let _ }
  in         { L.RangedToken T.In _ }
  where      { L.RangedToken T.Where _ }
  with       { L.RangedToken T.With _ }
  if         { L.RangedToken T.If _ }
  then       { L.RangedToken T.Then _ }
  else       { L.RangedToken T.Else _ }
  match      { L.RangedToken T.Match _ }

  '('        { L.RangedToken T.LParen _ }
  ')'        { L.RangedToken T.RParen _ }
  '['        { L.RangedToken T.LBrack _ }
  ']'        { L.RangedToken T.RBrack _ }
  '{'        { L.RangedToken T.LCurly _ }
  '}'        { L.RangedToken T.RCurly _ }

  ':'        { L.RangedToken T.Colon _ }
  ','        { L.RangedToken T.Comma _ }
  '->'       { L.RangedToken T.Arrow _ }
  '='        { L.RangedToken T.Equals _ }
  '|'        { L.RangedToken T.Pipe _ }
  '.'        { L.RangedToken T.Dot _ }
  '\\'       { L.RangedToken T.BackSlash _ } 

%%

identifier
  : id { unTok $1 (\range (T.Id name) -> AST.Name range name) }
 -- only to get the file compiling; we will remove this

assign
  : identifier '=' expr { AST.Assignment (info $1 <-> info $3) $1 $3 }

pairs
  :                                 { [] }
  | identifier ':' expr ',' pairs   { ($1, $3) : $5 }
  | identifier ':' expr             { [($1, $3)] }

record 
  : '{' pairs '}'   { AST.LRecord (L.rtRange $1 <-> L.rtRange $3) $2 }

listElements
  :                         { [] }
  | expr                    { [$1] }
  | expr ',' listElements   { $1 : $3 }

array 
  : '[' listElements ']'    { AST.LArray (L.rtRange $1 <-> L.rtRange $3) $2 }

tuple
  : '(' listElements ')'    { AST.LTuple (L.rtRange $1 <-> L.rtRange $3) $2 }

literal 
  : number     { unTok $1 (\range (T.Number int) -> AST.LInt range int) }
  | string     { unTok $1 (\range (T.String string) -> AST.LString range string) }
  | boolean    { unTok $1 (\range (T.Boolean boolean) -> AST.LBool range boolean) }
  | tuple      { $1 }
  | array      { $1 }
  | record     { $1 }



args
  :                  { [] }
  | identifier args  { $1 : $2 }

lambda
  : '|' args '->' expr { AST.Lambda (L.rtRange $1 <-> info $4) $2 $4 }


expr
  : assign   { AST.Assign $1 }
  | literal  { AST.Lit $1 }
  | lambda   { $1 }

{



-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok range) contructor = contructor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2



parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)



runSaga :: String -> Either String (AST.Expr L.Range)
runSaga input = L.runAlex (BS.pack input) parseSaga

}