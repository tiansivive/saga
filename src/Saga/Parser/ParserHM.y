{
module Saga.Parser.ParserHM
    ( runSagaExpr
    -- , runSagaScript
    , runSagaType
    -- , runSagaKind
    -- , runSagaDec
    , parseSagaExpr
    , parseSagaType
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
%name parseSagaType typeExpr
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

 -- COLLECTIONS
pairs
  :                                 { [] }
  | identifier ':' expr ',' pairs   { (P.keyValPair $1 $3) : $5 }
  | identifier ':' expr             { [P.keyValPair $1 $3] }

record 
  : '{' pairs '}'   { P.record $2 $1 $3 }

-- listElements
--   :                         { [] }
--   | expr                    { [$1] }
--   | expr ',' listElements   { $1 : $3 }

-- list 
--   : '[' listElements ']'    { (L.rtRange $1 <-> L.rtRange $3, HM.LList $2) }

tupleElems
  : ',' expr              { [$2] }
  | ',' expr tupleElems   { $2 : $3 }

tuple
  : '(' expr tupleElems ')'    { P.tuple ($2:$3) $1 $4 }


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
  | tuple      { $1 }
--   | list       { $1 }
  | record     { $1 }
  
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
  | expr '+' expr           { P.binaryOp $1 $2 $3 }
  | expr '-' expr           { P.binaryOp $1 $2 $3 }
  | expr '*' expr           { P.binaryOp $1 $2 $3 }
  | expr '/' expr           { P.binaryOp $1 $2 $3 }



-- Types
tpairs
  :                                     { [] }
  | identifier ':' typeExpr ',' tpairs  { (P.keyValPair $1 $3) : $5 }
  | identifier ':' typeExpr             { [P.keyValPair $1 $3] }

trecord 
  : '{' tpairs '}'   { P.tyRecord $2 $1 $3 }

ttupleElems
  : ',' typeExpr              { [$2] }
  | ',' typeExpr ttupleElems   { $2 : $3 }

ttuple
  : '(' typeExpr ttupleElems ')'    { P.tyTuple ($2:$3) $1 $4 }


type 
  : number     { P.number (HM.TLiteral . HM.LInt) $1 }
  | boolean    { P.boolean (HM.TLiteral . HM.LBool) $1 }
  | string     { P.string (HM.TLiteral . HM.LString) $1 }
  | ttuple     { $1 }
  | trecord    { $1 }
  
  -- | identifier '<' typeParams '>' { Types.TParametric (Types.Type $ resolveIdType $1) $3 }


typeAtom
  : type  { P.tyExpr $1 } 
  | identifier %shift  { P.tyIdentifier $1 }
  | '(' typeExpr ')' { P.tyParenthesised $2 $1 $3 }

typeArgs
  :                     { [] }
  | typeArgs typeAtom   { $1 ++ [$2] }

typeExpr 
  -- : if typeExpr then typeExpr else typeExpr { Types.TConditional (L.rtRange $1 <-> info $6) $2 $4 $6 }
  : typeExpr '->' typeExpr  { P.typeArrow $1 $3 }
  | '\\' params '->' typeExpr { P.typeLambda $2 $4 $1 }
  | typeAtom typeArgs '!' { P.typeFnApplication $1 $2 $3 }
  | typeAtom     { $1 }
  -- | qualifiers '.' typeExpr                          { Types.Type $ Types.TConstrained $1 [] $3 }
  -- | qualifiers '.' constraints '=>' typeExpr %shift  { Types.Type $ Types.TConstrained $1 $3 $5 }
  -- | implements identifier ':' typeExpr %shift { Types.Type (Types.TImplementation $2 $4 []) }
  -- | with args '=>' implements identifier ':' typeExpr %shift { Types.Type (Types.TImplementation $5 $7 $2) }

typeAnnotation
  : { Nothing }
  | ':' typeExpr { Just $2 }


-- Kinds






{








-- runSagaScript :: String -> Either String HM.Script
-- runSagaScript input = input `run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData HM.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaType :: String -> Either String (P.ParsedData HM.TypeExpr)
runSagaType input = input `P.run` parseSagaType

-- runSagaKind :: String -> Either String (Kinds.Kind L.Range)
-- runSagaKind input = input `run` parseSagaKind

-- runSagaDec :: String -> Either String HM.Declaration 
-- runSagaDec input = input `run` parseSagaDec

}