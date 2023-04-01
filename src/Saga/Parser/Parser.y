{
module Saga.Parser.Parser  
    ( runSagaScript
    , runSagaExpr 
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe  (Maybe (..), fromJust)
import Data.Monoid (First (..))
import Data.List (last)

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.AST.Syntax as AST

}

%name parseSagaScript script
%name parseSagaExpr expr
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
  '='        { L.RangedToken T.Equals _ }
  '|'        { L.RangedToken T.Pipe _ }
  '.'        { L.RangedToken T.Dot _ }
  '\\'       { L.RangedToken T.BackSlash _ } 



  newline         { L.RangedToken T.Newline _ }
  eof        { L.RangedToken T.EOF _ }



%right else in
%right '->'
%left '||'
%left '&&'

%nonassoc '=' "==" "!=" '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%nonassoc id number string boolean '(' ')' '[' ']' '{' '}'  
%nonassoc APP
%%




identifier
  : id { unTok $1 (\range (T.Id name) -> AST.Name range (BS.unpack name)) }

path
  : identifier          { [$1] }
  | identifier '.' path { $1 : $3 }

 -- COLLECTIONS
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

list 
  : '[' listElements ']'    { AST.LList (L.rtRange $1 <-> L.rtRange $3) $2 }

tupleElems
  : ',' expr                  { [$2] }
  | ',' expr tupleElems   { $2 : $3 }

tuple
  : '(' expr tupleElems ')'    { AST.LTuple (L.rtRange $1 <-> L.rtRange $4) ($2:$3) }


-- FUNCTIONS
args
  :                  { [] }
  | identifier args  { $1 : $2 }

lambda
  : '\\' args '->' expr { AST.Lambda (L.rtRange $1 <-> info $4) $2 $4 }

params
  :               { [] }
  | atom params   { $1 : $2 }

fnApplication 
  : atom params '!' { AST.FnApp (info $1 <-> L.rtRange $3) $1 $2 }
  
--CONTROL FLOW
controlFlow 
  : if expr then expr else expr { AST.IfElse (L.rtRange $1 <-> info $6) $2 $4 $6 }


-- BLOCKS
clause 
  --: with definition in expr    { AST.Clause (L.rtRange $1 <-> info $4) [$2] $4 }
  : with declarations  { $2 }

block
  : return expr   { [AST.Return (L.rtRange $1 <-> info $2) $2] }
  | expr block  { $1 : $2 }



--EXPRESSIONS
term 
  : number     { unTok $1 (\range (T.Number int) -> AST.LInt range int) }
  | string     { unTok $1 (\range (T.String string) -> AST.LString range string) }
  | boolean    { unTok $1 (\range (T.Boolean boolean) -> AST.LBool range boolean) }
  | tuple      { $1 }
  | list       { $1 }
  | record     { $1 }
  

atom
  : identifier              { AST.Identifier $1 }
  | term                    { AST.Term $1 }
  | '{' block '}'           { AST.Block (L.rtRange  $1 <-> L.rtRange $3) $2 }
  | '(' expr ')'            { AST.Parens (L.rtRange  $1 <-> L.rtRange $3) $2 }

expr
  
  : controlFlow             { $1 }
  | fnApplication           { $1 }
  | lambda                  { $1 }
  | clause in expr          { AST.Clause ((info $ head $1) <-> (info $ last $1)) $1 $3 }
  | atom %shift             { $1 }
  | identifier '=' expr     { AST.Assign $1 $3 }  

  | expr '+' expr           { binaryOp $1 $2 $3 }
  | expr '-' expr           { binaryOp $1 $2 $3 }
  | expr '*' expr           { binaryOp $1 $2 $3 }
  | expr '/' expr           { binaryOp $1 $2 $3 }

  --| expr '==' expr           { binaryOp $1 $2 $3 }
  -- | expr '!=' expr           { binaryOp $1 $2 $3 }
   | expr '<' expr           { binaryOp $1 $2 $3 }
   | expr '<=' expr           { binaryOp $1 $2 $3 }
   | expr '>' expr           { binaryOp $1 $2 $3 }
   | expr '>=' expr           { binaryOp $1 $2 $3 }
 
   | expr '||' expr           { binaryOp $1 $2 $3 }
   | expr '&&' expr           { binaryOp $1 $2 $3 }


-- TYPES
tpairs
  :                                 { [] }
  | identifier ':' typeExpr ',' tpairs   { ($1, $3) : $5 }
  | identifier ':' typeExpr             { [($1, $3)] }

trecord 
  : '{' tpairs '}'   { AST.TRecord (L.rtRange $1 <-> L.rtRange $3) $2 }

ttupleElems
  : ',' typeExpr                  { [$2] }
  | ',' typeExpr ttupleElems   { $2 : $3 }

ttuple
  : '(' typeExpr ttupleElems ')'    { AST.TTuple (L.rtRange $1 <-> L.rtRange $4) ($2:$3) }

type 
  : number { unTok $1 (\range (T.Number int) -> AST.TLiteral $ AST.LInt range int) }
  | string     { unTok $1 (\range (T.String string) -> AST.TLiteral $ AST.LString range string) }
  | boolean    { unTok $1 (\range (T.Boolean boolean) -> AST.TLiteral $ AST.LBool range boolean) }
  | ttuple      { $1 }
  | trecord     { $1 }
  | identifier   { AST.TIdentifier $1 }


typeExpr 
  : type { AST.Type $1 }
  | if typeExpr then typeExpr else typeExpr { AST.TConditional (L.rtRange $1 <-> info $6) $2 $4 $6 }
  | '(' typeExpr ')' {AST.TParens (L.rtRange  $1 <-> L.rtRange $3) $2}

typeAnnotation
  : { Nothing }
  | identifier ':' typeExpr { Just $3 } 

-- SCRIPT

dec 
  : typeAnnotation identifier '=' expr { declaration $2 $4 $1 }

declarations
  :                  { [] }
  | dec declarations { $1 : $2 }

moduleDef
  : module path where { AST.Mod (L.rtRange $1 <-> L.rtRange $3) ( map (\(AST.Name _ name) -> name) $2 ) } -- TODO: extract to named fn

importMod
  : import path { AST.Import (L.rtRange $1 <-> (info $ last $2)) ( map (\(AST.Name _ name) -> name) $2 ) }

imports
  :                    { [] }
  | importMod imports  { $1 : $2 }
 
script
  : moduleDef declarations imports { AST.Script (info $1 <-> (info $ last $2)) $1 $2 $3 }


{

declaration :: AST.Name L.Range -> AST.Expr L.Range -> Maybe (AST.TypeExpr L.Range) -> AST.Declaration L.Range
declaration id expr tyAnn = AST.Define (info id <-> info expr) id expr tyAnn
  

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok range) contructor = contructor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure


binaryOp :: AST.Expr L.Range -> L.RangedToken -> AST.Expr L.Range -> AST.Expr L.Range 
binaryOp expr1 op expr2 = AST.FnApp (info expr1 <-> info expr2) (unTok op (\range (T.Operator char) -> AST.Identifier (AST.Name range (BS.unpack char)))) [expr1, expr2]


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



runSagaScript :: String -> Either String (AST.Script L.Range)
runSagaScript input = L.runAlex (BS.pack input) parseSagaScript

runSagaExpr :: String -> Either String (AST.Expr L.Range)
runSagaExpr input = L.runAlex (BS.pack input) parseSagaExpr

}