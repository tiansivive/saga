{
module Saga.Parser.ParserHM
    ( runSagaExpr
    -- , runSagaScript
    -- , runSagaType
    -- , runSagaKind
    -- , runSagaDec
    , parseSagaExpr
    -- , parseSagaType
    -- , parseSagaKind
    -- , parseSagaDec
    ) where

import Data.Char (isLower)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe  (Maybe (..), fromJust)
import Data.Monoid (First (..))
import Data.List (last, head)

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.Parser.ParsingInfo as P
import           Saga.Parser.ParsingInfo ((<->))

import qualified Saga.AST.TypeSystem.Types as Types
import qualified Saga.AST.TypeSystem.Kinds as Kinds

import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

import qualified Saga.AST.Scripts as Scripts

}

-- %name parseSagaScript script
%name parseSagaExpr expr
-- %name parseSagaType typeExpr
-- %name parseSagaKind kindExpr
-- %name parseSagaDec dec
%tokentype { L.RangedToken }
%error { P.parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { P.lexer } { L.RangedToken T.EOF _ }


%token
  -- Identifiers
  id { L.RangedToken (T.Id _) _ }
  -- Constants
  number     { L.RangedToken (T.Number _) _ }
  string     { L.RangedToken (T.String _) _ }
  boolean    { L.RangedToken (T.Boolean _) _ }

  -- operators 
  '!'         { L.RangedToken (T.Operator "!") _ } 
  '+'         { L.RangedToken (T.Operator "+") _ } 
  '-'         { L.RangedToken (T.Operator "-") _ } 
  '*'         { L.RangedToken (T.Operator "*") _ } 
  '/'         { L.RangedToken (T.Operator "/") _ } 

  '=='         { L.RangedToken (T.Operator "==") _ } 
  '!='         { L.RangedToken (T.Operator "!=") _ } 
  '<'         { L.RangedToken (T.Operator "<") _ } 
  '<='         { L.RangedToken (T.Operator "<=") _ } 
  '>'         { L.RangedToken (T.Operator ">") _ } 
  '>='         { L.RangedToken (T.Operator ">=") _ } 

  '||'         { L.RangedToken (T.Operator "||") _ } 
  '&&'         { L.RangedToken (T.Operator "&&") _ } 

  op         { L.RangedToken (T.Operator _) _ }

  -- Keywords
  let        { L.RangedToken T.Let _ }
  in         { L.RangedToken T.In _ }
  where      { L.RangedToken T.Where _ }
  with       { L.RangedToken T.With _ }
  if         { L.RangedToken T.If _ }
  then       { L.RangedToken T.Then _ }
  else       { L.RangedToken T.Else _ }
  match      { L.RangedToken T.Match _ }
  return     { L.RangedToken T.Return _ }

  -- Data
  data       { L.RangedToken T.Data _ }

  -- Types
  ty         { L.RangedToken T.Type _ }
  alias      { L.RangedToken T.Alias _ }
  kind       { L.RangedToken T.Kind _ }
  forall     { L.RangedToken T.Forall _ }
  exists     { L.RangedToken T.Exists _ }
  proof      { L.RangedToken T.Proof _ }
  infer      { L.RangedToken T.Infer _ }

  -- Interfaces

  protocol   { L.RangedToken T.Protocol _ }
  interface  { L.RangedToken T.Interface _ }
  instance   { L.RangedToken T.Instance _ }
  implements { L.RangedToken T.Implements _ }

  -- Modules
  module     { L.RangedToken T.Module _ }
  import     { L.RangedToken T.Import _ }

  '('        { L.RangedToken T.LParen _ }
  ')'        { L.RangedToken T.RParen _ }
  '['        { L.RangedToken T.LBrack _ }
  ']'        { L.RangedToken T.RBrack _ }
  '{'        { L.RangedToken T.LCurly _ }
  '}'        { L.RangedToken T.RCurly _ }

  ':'        { L.RangedToken T.Colon _ }
  ','        { L.RangedToken T.Comma _ }
  '->'       { L.RangedToken T.Arrow _ }
  '<-'       { L.RangedToken T.BackArrow _ }
  '=>'       { L.RangedToken T.FatArrow _ }
  '|->'      { L.RangedToken T.PipeArrow _ }
  '='        { L.RangedToken T.Equals _ }
  '|'        { L.RangedToken T.Pipe _ }
  '.'        { L.RangedToken T.Dot _ }
  '::'       { L.RangedToken T.Section _ }
  '\\'       { L.RangedToken T.BackSlash _ } 



  newline    { L.RangedToken T.Newline _ }
  eof        { L.RangedToken T.EOF _ }



%right else in
%right '->'
%right '.'
%left '||'
%left '&&'

%nonassoc '='
%nonassoc "==" "!=" '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%nonassoc id number string boolean '(' ')' '[' ']' '{' '}'  
%nonassoc APP
%%




identifier
  : id { P.identifier $1 HM.Identifier }

-- path
--   : identifier          { \(range, id) -> (range, [$1]) }
--   | path '.' identifier { \(range, path) _ (range', id) -> (range <-> range') (path ++ [id])}

--  -- COLLECTIONS
-- pairs
--   :                                 { [] }
--   | identifier ':' expr ',' pairs   { ($1, $3) : $5 }
--   | identifier ':' expr             { [($1, $3)] }

-- record 
--   : '{' pairs '}'   { (L.rtRange $1 <-> L.rtRange $3, HM.LRecord $2) }

-- listElements
--   :                         { [] }
--   | expr                    { [$1] }
--   | expr ',' listElements   { $1 : $3 }

-- list 
--   : '[' listElements ']'    { (L.rtRange $1 <-> L.rtRange $3, HM.LList $2) }

-- tupleElems
--   : ',' expr                  { [$2] }
--   | ',' expr tupleElems   { $2 : $3 }

-- tuple
--   : '(' expr tupleElems ')'    { (L.rtRange $1 <-> L.rtRange $4, HM.LTuple ($2:$3)) }


-- FUNCTIONS
params
  :                   { [] }
  | params identifier { $1 ++ [$2] }

args
  :               { [] }
  | args atom     { $1 ++ [$2] }

fnApplication 
  : atom args '!' { P.fnApplication $1 $2 $3 }
 
--CONTROL FLOW
controlFlow 
  : if expr then expr else expr { P.controlFlow $1 $2 $4 $6 }


-- BLOCKS
-- block
--   : return expr   { [HM.Return (L.rtRange $1 <-> info $2) $2] }
--   | expr block  { $1 : $2 }


--EXPRESSIONS
term 
  : number     { P.number HM.LInt $1 }
  | boolean    { P.boolean HM.LBool $1 }
  | string     { P.string HM.LString $1 }
--   | tuple      { $1 }
--   | list       { $1 }
--   | record     { $1 }
  
atom
  : identifier              { $1 }
  -- | atom '.' path           { HM.FieldAccess (info $1 <-> (info $ last $3)) $1 $3 }
  | term                    { P.term $1 }
  -- | '{' block '}'           { HM.Block (L.rtRange  $1 <-> L.rtRange $3) $2 }
  | '(' expr ')'            { P.parenthesised $2 $1 $3 }



assignment 
  : identifier '=' expr     { P.assignment $1 $3 }  

expr
  
  : controlFlow             { $1 }    
  | fnApplication           { $1 }--{ Syntax.FnApp (info $ head $1 <-> info $ last $1) (head $1) (tail $1) }
  | '\\' params '->' expr   { P.lambda $2 $4 $1 }
  -- | with assignments in expr { Syntax.Clause (L.rtRange $1 <-> info $4) $2 $4 }
  | atom %shift             { $1 }
  -- | identifier '=' expr     { Syntax.Assign $1 $3 }  





{








-- binaryOp :: HM.Expr -> L.RangedToken -> HM.Expr -> HM.Expr
-- binaryOp expr1 op expr2 = HM.FnApp (info expr1 <-> info expr2) (unTok op (\range (T.Operator char) -> HM.Identifier (HM.Name range (BS.unpack char)))) [expr1, expr2]





-- runSagaScript :: String -> Either String HM.Script
-- runSagaScript input = input `run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData HM.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

-- runSagaType :: String -> Either String HM.TypeExpr
-- runSagaType input = input `run` parseSagaType

-- runSagaKind :: String -> Either String (Kinds.Kind L.Range)
-- runSagaKind input = input `run` parseSagaKind

-- runSagaDec :: String -> Either String HM.Declaration 
-- runSagaDec input = input `run` parseSagaDec

}