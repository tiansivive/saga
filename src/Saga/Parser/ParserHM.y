{
module Saga.Parser.ParserHM
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
import           Saga.Parser.ParsingInfo ((<->))


import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

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
%right '->'
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

listElements
  :                         { [] }
  | expr                    { [$1] }
  | expr ',' listElements   { $1 : $3 }

list 
  : '[' listElements ']'    { P.list $2 $1 $3 }

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


-- PATTERN MATCHING

patListElems
  : identifier                    { [$1] }
  | identifier ',' patListElems   { $1 : $3 }

patTupleElems
  : ',' identifier                { [$2] }
  | ',' identifier patTupleElems  { $2 : $3 }

patRecordKeys
  :                               { [] }
  | identifier                    { [$1] }
  | identifier ',' patRecordKeys  { $1 : $3 }

patRest
  :                 { Nothing }
  | '|' identifier  { Just $2 }

patData
  : identifier ':'     { [$1]}
  | patData identifier { $1 ++ [$2] }

pattern 
  : identifier                        { P.pattern $ P.Var $1   }
  | term                              { P.pattern $ P.Term $ fmap HM.Term $1   }
  | '(' identifier patTupleElems ')'  { P.pattern $ P.Tuple ($2 : $3) }
  | '[' ']'                           { P.pattern $ P.List [] Nothing }
  | '[' patListElems patRest ']'      { P.pattern $ P.List $2 $3 }
  | '{' patRecordKeys patRest '}'     { P.pattern $ P.Record $2 $3 }
  | patData                           { P.pattern $ P.Tagged $1 }


--EXPRESSIONS
term 
  : number     { P.number HM.LInt $1 }
  | string     { P.string HM.LString $1 }
  | boolean    { P.boolean HM.LBool $1 } 

  
atom
  : identifier              { $1 }
  | term                    { P.term $1 }
  | tuple                   { $1 }
  | list                    { $1 }
  | record                  { $1 }
  -- | '{' block '}'           { HM.Block (L.rtRange  $1 <-> L.rtRange $3) $2 }
  | '(' expr ')'            { P.parenthesised $2 $1 $3 }


cases
  : '|' pattern '->' expr        { [P.matchCase $2 $4] }
  | cases '|' pattern '->' expr  { $1 ++ [P.matchCase $3 $5] }

matchExpr
  : match expr cases %shift { P.match $2 $3 }


binding
  : identifier '=' expr  %prec RIGHT { P.binding $1 $3 }

bindings
  : binding               { [$1] }
  | bindings ',' binding  { $1 ++ [$3] }

expr
  : controlFlow             { $1 }    
  | matchExpr               { $1 }
  | fnApplication           { $1 }
  | '\\' params '->' expr   { P.lambda $2 $4 $1 }
  | atom %shift             { $1 }
  | '.' atom                { P.dotLambda $2 }

  | expr '.' identifier     { P.binaryOp $1 $2 $3 }
  | expr '+' expr           { P.binaryOp $1 $2 $3 }
  | expr '-' expr           { P.binaryOp $1 $2 $3 }
  | expr '*' expr           { P.binaryOp $1 $2 $3 }
  | expr '/' expr           { P.binaryOp $1 $2 $3 }
  | expr '^' expr           { P.binaryOp $1 $2 $3 }
  | expr '||' expr          { P.binaryOp $1 $2 $3 }
  | expr '&&' expr          { P.binaryOp $1 $2 $3 }
  | expr '==' expr          { P.binaryOp $1 $2 $3 }
  | expr '!=' expr          { P.binaryOp $1 $2 $3 }
  | expr '<' expr           { P.binaryOp $1 $2 $3 }
  | expr '>' expr           { P.binaryOp $1 $2 $3 }
  | expr '<=' expr          { P.binaryOp $1 $2 $3 }
  | expr '>=' expr          { P.binaryOp $1 $2 $3 }
  | expr '|>' expr          { P.binaryOp $1 $2 $3 }
  | expr '<|' expr          { P.binaryOp $1 $2 $3 }
  | expr '++' expr          { P.binaryOp $1 $2 $3 }


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
  : number     { P.number (HM.TTerm . HM.LInt) $1 }
  | boolean    { P.boolean (HM.TTerm . HM.LBool) $1 }
  | string     { P.string (HM.TTerm . HM.LString) $1 }
  | ttuple     { $1 }
  | trecord    { $1 }
  
  -- | identifier '<' typeParams '>' { Types.TClosure (Types.Type $ resolveIdType $1) $3 }


typeAtom
  : type  { $1 } 
  | identifier %shift  { P.tyIdentifier $1 }
  | '(' typeExpr ')' { P.tyParenthesised $2 $1 $3 }

typeArgs
  :                     { [] }
  | typeArgs typeAtom   { $1 ++ [$2] }


tbinding
  : identifier '=' typeExpr         { P.tyBinding P.Id $1 $3 }
  | identifier implements typeExpr  { P.tyBinding P.Impl $1 $3 }
  | identifier '|->' typeExpr       { P.tyBinding P.Subtype $1 $3 }
  | identifier '|' typeExpr         { P.tyBinding P.Refinement $1 $3 }

tbindings
  : tbinding                { [$1] }
  | tbindings ';' tbinding  { $1 ++ [$3] }

tagged
  : identifier ':' typeExpr    %shift { P.tagged $1 $3 }


union
  --:                               { }
  : '|' typeExpr            { [$2] }
  | union '|' typeExpr      { $1 ++ [$3] }

typeExpr 
  : typeAtom                              { $1 }
  | tagged                                { $1 }
  | union                       %shift    { P.typeUnion $1 }
  | typeExpr '->' typeExpr      %shift    { P.typeArrow $1 $3 }                               
  | '\\' params '=>' typeExpr   %shift    { P.typeLambda $2 $4 $1 }
  | typeAtom typeArgs '!'                 { P.typeFnApplication $1 $2 $3 }
  
  

  
  -- : if typeExpr then typeExpr else typeExpr { Types.TConditional (L.rtRange $1 <-> info $6) $2 $4 $6 }
  -- | qualifiers '.' typeExpr                          { Types.Type $ Types.TConstrained $1 [] $3 }
  -- | qualifiers '.' constraints '=>' typeExpr %shift  { Types.Type $ Types.TConstrained $1 $3 $5 }
  -- | implements identifier ':' typeExpr %shift { Types.Type (Types.TImplementation $2 $4 []) }
  -- | with args '=>' implements identifier ':' typeExpr %shift { Types.Type (Types.TImplementation $5 $7 $2) }

typeAnnotation
  :                                       { Nothing }
  | ':' typeExpr                          { Just $2 }
  | ':' typeExpr where tbindings          { Just $ P.typeClause $2 $4 } 
  | ':' instance identifier ':' typeExpr  { Just $ P.implementation $3 $5 } 




kindExpr
  : kindExpr '->' kindExpr  %prec RIGHT { P.kindArrow $1 $3 }
  | '(' kindExpr ')'                    { $2 }  
  | identifier                          { P.kindId $1 }



kindAnnotation
  : { Nothing }
  | '::' kindExpr { Just $2 }

-- Data

dataExpr
  : identifier ':' typeExpr { P.dataExpr $1 $3 }

dataExprs
    : dataExpr { [$1] }
    | dataExprs '|' dataExpr { $1 ++ [$3] }


-- Decs

dec 
  : let identifier typeAnnotation kindAnnotation '=' expr                 { P.letdec $2 $3 $4 $6 }
  | let identifier typeAnnotation kindAnnotation '=' expr where bindings  { P.letdec $2 $3 $4 (P.clause $6 $8) }
  | data identifier kindAnnotation '=' dataExprs                          { P.dataType $2 $3 $5 [] }
  | data identifier kindAnnotation '=' dataExprs where tbindings          { P.dataType $2 $3 $5 $7 }
  | ty identifier kindAnnotation '=' typeExpr                             { P.typeDef $2 $3 $5 }
  | ty identifier kindAnnotation '=' typeExpr where tbindings             { P.typeDef $2 $3 (P.typeClause $5 $7) }
 

declarations
  : dec              { [$1] }
  | declarations dec { $1 ++ [$2] }

script
  : declarations { P.script $1 }


{


runSagaScript :: String -> Either String (P.ParsedData HM.Script)
runSagaScript input = input `P.run` parseSagaScript

runSagaExpr :: String -> Either String (P.ParsedData HM.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

runSagaType :: String -> Either String (P.ParsedData HM.TypeExpr)
runSagaType input = input `P.run` parseSagaType

runSagaKind :: String -> Either String (P.ParsedData HM.Kind)
runSagaKind input = input `P.run` parseSagaKind

runSagaDec :: String -> Either String (P.ParsedData HM.Declaration)
runSagaDec input = input `P.run` parseSagaDec

}