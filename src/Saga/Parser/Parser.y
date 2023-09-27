{
module Saga.Parser.Parser where


import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T


import qualified Saga.Language.Core.Literals as PL

import qualified Saga.Parser.Expr as PE
import qualified Saga.Parser.Types as PT
import qualified Saga.Parser.Shared as P
import qualified Saga.Parser.ParsingInfo as P



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
  HOLE { L.RangedToken (T.Hole _) _ }

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
  
  '`'          { L.RangedToken (T.Operator "<|") _ } 
  '#'          { L.RangedToken (T.Operator "#") _ } 
  op           { L.RangedToken (T.Operator _) _ }

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


%nonassoc ',' ';' '=' '==' '!=' '&&' '||' '<' '>' '<=' '>=' '|>' '<|' 
%left '+' '-' '++'
%left '*' '/'
%left '^'

%nonassoc id number string boolean '(' ')' '[' ']' '{' '}'  
%nonassoc APP
%right RIGHT
%left LEFT
%%


identifier
  : id  { P.identifier $1 PE.Identifier }

hole 
  : HOLE { P.identifier $1 PE.Hole }


--EXPRESSIONS
expr 
  : expr1   %shift { $1 }
  --| expr '::' typeAtom { $1 }

expr1 
  : expr2                                              %shift { $1 }
  | expr1 operator expr2                               %shift { P.binaryOp $1 $2 $3 }

expr2 
  : expr3                                              %shift { $1 }
  | expr2 '`' exprBacktick '`' expr3                   %shift { P.infixApplication $3 [$1, $5] }

exprBacktick
  : expr3                                                     { $1 }
  | exprBacktick operator expr3                               { P.binaryOp $1 $2 $3 }

expr3
  : expr4                                              %shift { $1 }
  | expr3 expr4                                        %shift { P.fnApplication $1 [$2] }
  | expr3 '!'                                                 { P.parenthesised $1 $2 $2 }

expr4
  : expr5                                                     { $1 }
  | expr4 '.' identifier                               %shift { P.binaryOp $1 $2 $3 }
  
expr5
  : expr6                                                     { $1 }
  | if expr then expr else expr                               { P.controlFlow $1 $2 $4 $6 }
  | match expr cases                                   %shift { P.match $2 $3 }       
  | '\\' manyOrEmpty(pattern) '->' expr                       { P.lambda (fmap P.pattern $2) $4 $1 }
  | '.' identifier                                            { P.dotLambda $2 } 
  
expr6
  : exprAtom                                                  { $1 }
  | '{' block '}'                                             { P.block $2 $1 $3 }
  
block
  : separated(statement, ';') trailing(';')                   { $1 }

statement
  : manyOrEmpty(identifier) '<-' expr                  %shift { P.backcall (fmap (P.pattern . P.Var) $1) $3 }
  | '\\' manyOrEmpty(pattern) '<-' expr                %shift { P.backcall (fmap P.pattern $2) $4 }
  | expr                                                      { fmap PE.Procedure $1 }
  | return expr                                               { P.returnStmt $2 $1 }
  | letdec                                                    { fmap PE.Declaration $1 } 

exprAtom 
  : hole                                                      { $1 }
  | identifier                                         %shift { $1 }
  | term                                                      { P.term $1 }
  | tuple                                                     { $1 }
  | list                                                      { $1 }
  | record                                                    { $1 }
  | '(' expr ')'                                              { P.parenthesised $2 $1 $3 }

term 
  : number                                                    { P.number PL.LInt $1 }
  | string                                                    { P.string PL.LString $1 }
  | boolean                                                   { P.boolean PL.LBool $1 } 

-- COLLECTIONS
record 
  : '{' separatedOrEmpty(keyValPair, ',') trailing(',') '}'   { P.record $2 $1 $4 }

keyValPair
  : identifier ':' expr                                       { P.keyValPair $1 $3 }

list 
  : '[' separatedOrEmpty(expr, ',') ']'                       { P.list $2 $1 $3 }

tuple
  : '(' separated(expr, ',') ')'                              { P.tuple $2 $1 $3 }

-- PATTERNS
cases
  : '|' pattern '->' expr                                            { [P.matchCase (P.pattern $2) $4] }
  | cases '|' pattern '->' expr                                      { $1 ++ [P.matchCase (P.pattern $3) $5] }

pattern
  : patternAtom                                                      { $1 }
  | identifier ':' many(pattern)                              %shift { P.Tagged $1 $3 }
   
patternAtom 
  : identifier                                                %shift { P.Var $1 }
  | hole                                                      %shift { P.Hole $1 }
  | term                                                      %shift { P.Term $ fmap PE.Literal $1   }
  | '(' separated(pattern, ',') trailing(patRest) ')'                { P.Tuple $2 $3 }
  | '[' separatedOrEmpty(pattern, ',') trailing(patRest) ']'         { P.List $2 $3 }
  | '{' patRecordKeys trailing(patRest) '}'                          { P.Record $2 $3 }
  | '(' pattern ')'                                                  { $2 }

patRecordKeys
  :                                                                  { [] }
  | identifier                                                       { [($1, Nothing)] }
  | identifier ':' pattern                                           { [($1, Just $3)] }
  | identifier ',' patRecordKeys                                     { ($1, Nothing) : $3 }
  | identifier ':' pattern ',' patRecordKeys                         { ($1, Just $3) : $5 }

patRest
  : '|' identifier                                                   { $2 }



-- | TYPES
typeExpr 
  : typeExpr1                                         { $1 }
  | union                                      %shift { P.typeUnion $1 }

typeExpr1
  : typeExpr2                                  %shift { $1 }
  | identifier ':' typeExpr                           { P.tagged $1 $3 }

union
  : '|' typeExpr                                      { [$2] }
  | union '|' typeExpr                                { $1 ++ [$3] }

typeExpr2
  : typeExpr3                                  %shift { $1 }                               
  | typeExpr3 '->' typeExpr                           { P.typeArrow $1 $3 }                               
 
typeExpr3
  : typeExpr4                                  %shift { $1 }
  | instance identifier ':' typeExpr           %shift { P.typeProtocolImplementation (P.tyIdentifier $2) $4 }

typeExpr4
  : typeExpr5                                         { $1 }
  | typeExpr4 typeExpr5                               { P.typeFnApplication $1 [$2] }
  | typeExpr4 '!'                                     { P.tyParenthesised $1 $2 $2 } 

typeExpr5 
  : typeAtom { $1 }
  | '\\' many(identifier) '=>' typeExpr               { P.typeLambda ($2) $4 $1 }


typeAtom
  : number                                            { P.number (PT.TLiteral . PL.LInt) $1 }
  | boolean                                           { P.boolean (PT.TLiteral . PL.LBool) $1 }
  | string                                            { P.string (PT.TLiteral . PL.LString) $1 }
  | identifier                                        { P.tyIdentifier $1 }
  | '(' typeExpr ')'                                  { P.tyParenthesised $2 $1 $3 }
  | '(' separated(typeExpr, ',') ')'                  { P.tyTuple $2 $1 $3 }
  | '{' tpairs '}'                                    { P.tyRecord $2 $1 $3 }
  
tpairs
  :                                                   { [] }
  | identifier ':' typeExpr ',' tpairs                { (P.keyValPair $1 $3) : $5 }
  | identifier ':' typeExpr                           { [P.keyValPair $1 $3] }


-- Decs
script
  : many(dec)                                         { P.script $1 }
dec 
  : letdec                                                                                              { $1 }
  | let identifier typeAnnotation kindAnnotation '=' expr where separated(binding, ',')                 { P.letdec $2 $3 $4 (P.clause $6 $8) }
  | data identifier kindAnnotation '=' separated(dataExpr, '|')                                         { P.dataType $2 $3 $5 [] }
  | data identifier kindAnnotation '=' separated(dataExpr, '|') where separated(tbinding, ',')          { P.dataType $2 $3 $5 $7 }
  | ty identifier kindAnnotation '=' typeExpr                                                           { P.typeDef $2 $3 $5 }
  | ty identifier kindAnnotation '=' typeExpr where separated(tbinding, ',')                            { P.typeDef $2 $3 (P.typeClause $5 $7) }

letdec
  : let identifier typeAnnotation kindAnnotation '=' expr                                               { P.letdec $2 $3 $4 $6 }

dataExpr
  : identifier ':' typeExpr         { P.dataExpr $1 $3 }


binding
  : identifier '=' expr             { P.binding $1 $3 }

tbinding
  : identifier '=' typeExpr         { P.tyBinding P.Id $1 $3 }
  | identifier implements typeExpr  { P.tyBinding P.Impl $1 $3 }
  | identifier '|->' typeExpr       { P.tyBinding P.Subtype $1 $3 }
  | identifier '|' typeExpr         { P.tyBinding P.Refinement $1 $3 }


typeAnnotation
  :                                                 { Nothing }
  | ':' typeExpr                                    { Just $2 }
  | ':' typeExpr where separated(tbinding, ',')     { Just $ P.typeClause $2 $4 } 
  | ':' instance identifier ':' typeExpr            { Just $ P.implementation $3 $5 } 

kindAnnotation
  :                                                 { Nothing }
  | '::' kindExpr                                   { Just $2 }

kindExpr
  : kindExpr2                                %shift { $1 }
  | kindExpr2 '->' kindExpr                         { P.kindArrow $1 $3 }
  
kindExpr2
  : kindAtom                                        { $1 }
  | identifier kindExpr                             { P.kindApplication $1 $2 }
  
kindAtom
  : identifier                                      { P.kindId $1 }
  | '(' kindExpr ')'                                { $2 }  


-- UTILITIES
many(a) 
  : a           { [$1] }
  | many(a) a   { $1 ++ [$2] }

manyOrEmpty(a) 
  :            { [] }
  | many(a)    { $1 }

separated(a, separator)
  : a %shift                            { [$1] }
  | separated(a, separator) separator a { $1 ++ [$3] }

separatedOrEmpty(a, separator) 
  :                                { [] }
  | separated(a, separator) %shift { $1 }

defaultSeparator
  : ',' { $1 }
  | ';' { $1 }

trailing(separator)
  : %shift    { Nothing }
  | separator { Just $1 }

delimited(start, rule, separator, end) 
  : start end { [] }
  | start sep(rule, separator) end { $2 }

operator
  : '+'           { $1 }
  | '-'           { $1 }
  | '*'           { $1 }
  | '/'           { $1 }
  | '^'           { $1 }
  | '||'          { $1 }
  | '&&'          { $1 }
  | '=='          { $1 }
  | '!='          { $1 }
  | '<'           { $1 }
  | '>'           { $1 }
  | '<='          { $1 }
  | '>='          { $1 }
  | '|>'          { $1 }
  | '<|'          { $1 }
  | '++'          { $1 }

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