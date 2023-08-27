{
module Saga.Parser.Parser
    ( runSagaExpr
    , runSagaScript
    , runSagaType
    , runSagaKind
    , runSagaDec
    , parseSagaExpr
    , parseSagaType
    , parseSagaKind
    , parseSagaDec
    ) where


import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T

import qualified Saga.Parser.ParsingInfo as P
import qualified Saga.Parser.Shared  as P
import qualified Saga.Parser.Expr as PE
import qualified Saga.Parser.Literals as PL
import qualified Saga.Parser.Types as PT



}

%name parseSagaScript script
%name parseSagaExpr expr
%name parseSagaType typeExpr
%name parseSagaKind kindExpr
%name parseSagaDec dec
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
  '^'         { L.RangedToken (T.Operator "^") _ } 

  '++'        { L.RangedToken (T.Operator "++") _ } 

  '=='         { L.RangedToken (T.Operator "==") _ } 
  '!='         { L.RangedToken (T.Operator "!=") _ } 
  '<'         { L.RangedToken (T.Operator "<") _ } 
  '<='         { L.RangedToken (T.Operator "<=") _ } 
  '>'         { L.RangedToken (T.Operator ">") _ } 
  '>='         { L.RangedToken (T.Operator ">=") _ } 

  '||'         { L.RangedToken (T.Operator "||") _ } 
  '&&'         { L.RangedToken (T.Operator "&&") _ } 

  '|>'         { L.RangedToken (T.Operator "|>") _ } 
  '<|'         { L.RangedToken (T.Operator "<|") _ } 

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
  ';'        { L.RangedToken T.SemiColon _ }
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
%right '|'
%right '->' '<-'
%right '.'


%nonassoc ',' ';' '=' '==' '!=' '&&' '||' '<' '>' '<=' '>=' '|>' '<|' 
%left '+' '-' '++'
%left '*' '/'
%left '^'

%nonassoc id number string boolean '(' ')' '[' ']' '{' '}'  
%nonassoc APP
%right RIGHT
%left LEFT
%%


{


runSagaScript :: String -> Either String (P.ParsedData PE.Script)
runSagaScript input = input `P.run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData PE.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaType :: String -> Either String (P.ParsedData PT.TypeExpr)
runSagaType input = input `P.run` parseSagaType

runSagaKind :: String -> Either String (P.ParsedData PT.Kind)
runSagaKind input = input `P.run` parseSagaKind

runSagaDec :: String -> Either String (P.ParsedData PE.Declaration)
runSagaDec input = input `P.run` parseSagaDec

}