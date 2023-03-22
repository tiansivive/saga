{
module Saga.Parser.Parser where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Saga.Lexer.Lexer as L
import qualified Saga.AST.Syntax as AST

}

%name parseSaga expr
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%token
  -- Identifiers
  id { L.RangedToken (L.Id _) _ }
  -- Constants
  number     { L.RangedToken (L.Number _) _ }
  string     { L.RangedToken (L.String _) _ }
  boolean    { L.RangedToken (L.Boolean _) _ }

  -- Keywords
  let        { L.RangedToken L.Let _ }
  in         { L.RangedToken L.In _ }
  where      { L.RangedToken L.Where _ }
  with       { L.RangedToken L.With _ }
  if         { L.RangedToken L.If _ }
  then       { L.RangedToken L.Then _ }
  else       { L.RangedToken L.Else _ }
  match      { L.RangedToken L.Match _ }

  '('        { L.RangedToken L.LParen _ }
  ')'        { L.RangedToken L.RParen _ }
  '['        { L.RangedToken L.LBrack _ }
  ']'        { L.RangedToken L.RBrack _ }

  ':'        { L.RangedToken L.Colon _ }
  ','        { L.RangedToken L.Comma _ }
  '->'       { L.RangedToken L.Arrow _ }
  '='        { L.RangedToken L.Equals _ }
  '|'        { L.RangedToken L.Pipe _ }
  '.'        { L.RangedToken L.Dot _ }

%%

identifier
  : id { unTok $1 (\range (L.Id name) -> AST.Name range name) }
 -- only to get the file compiling; we will remove this

assign
  : identifier '=' expr { AST.Assignment (info $1 <-> info $3) $1 $3 }


literal 
  : number    { unTok $1 (\range (L.Number int) -> AST.LInt range int) }
  | string     { unTok $1 (\range (L.String string) -> AST.LString range string) }
  | boolean    { unTok $1 (\range (L.Boolean boolean) -> AST.LBool range boolean) }

expr
  : assign   { AST.Assign $1 }
  | literal  { AST.Lit $1 }


{



-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
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

}