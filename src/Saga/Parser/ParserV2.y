{
module Saga.Parser.ParserV2 where


import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T


import qualified Saga.Parser.Expr as PE
import qualified Saga.Parser.Literals as PL
import qualified Saga.Parser.Types as PT
import qualified Saga.Parser.Shared as P
import qualified Saga.Parser.ParsingInfo as P



}


%name parseSagaExpr expr
%partial parseBlock block
%partial parseStatement statement
--%partial parseDoNext doNext

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
  
  '`'         { L.RangedToken (T.Operator "<|") _ } 

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
  : expr1 %shift { $1 }
  --| expr1 '::' typeAnnotation { $1 }

expr1 
  : expr2 %shift { $1 }
  | expr1 '+' expr2 %shift { P.binaryOp $1 $2 $3 }
 

expr2 
  : expr3 %shift { $1 }
  | expr2 '`' exprBacktick '`' expr3 %shift { P.infixApplication $3 [$1, $5] }

exprBacktick
  : expr3 { $1 }
  | exprBacktick '+' expr3 { P.binaryOp $1 $2 $3 }

expr3
  : expr4 %shift        { $1 }
  | expr3 expr4 %shift  { P.fnApp $1 [$2] }
  --| expr3 expr4 '!'     { P.fnApp $1 [$2] }

expr4
  : expr5  { $1 }
  | expr4 '.' identifier %shift  { P.binaryOp $1 $2 $3 }

expr5
  : expr6 { $1 }
  | if expr then expr else expr { P.controlFlow $1 $2 $4 $6 }
  | match expr cases %shift { P.match $2 $3 }       
  | '\\' params '->' expr   { P.lambda $2 $4 $1 }
  | '.' identifier { P.dotLambda $2 } 
  
params
  :                   { [] }
  | params identifier { $1 ++ [$2] }
  
expr6
  : exprAtom { $1 }
  | '{' block '}' { P.block $2 $1 $3 }
  
block
  : returnStmt        { [$1] }
  | stmts returnStmt  { $1 ++ [$2] }

returnStmt 
  : return expr ';'   { P.returnStmt $2 $1 }

stmts
  : statement       { [$1] }
  | stmts statement { $1 ++ [$2] }

-- decStmt
statement
  : backcall ';'                     { $1 }
  | expr ';'  %shift                 { fmap PE.Procedure $1 }
  --| letdec                       { fmap PE.Declaration $1 } 

backcall
  : identifier '<-' expr            { P.backcall [P.pattern $ P.Var $1] $3 }
  -- | identifier '<-' expr           { P.backcall $1 $3 }


exprAtom 
  : hole                    { $1 }
  | identifier              { $1 }
  | term                    { P.term $1 }
  | tuple                   { $1 }
  | list                    { $1 }
  | record                  { $1 }
  | '(' expr ')'            { P.parenthesised $2 $1 $3 }

term 
  : number     { P.number PL.LInt $1 }
  | string     { P.string PL.LString $1 }
  | boolean    { P.boolean PL.LBool $1 } 

 -- COLLECTIONS
record 
  : '{' pairs '}'   { P.record $2 $1 $3 }
pairs
  :                                 { [] }
  | identifier ':' expr ',' pairs   { (P.keyValPair $1 $3) : $5 }
  | identifier ':' expr             { [P.keyValPair $1 $3] }

list 
  : '[' listElements ']'    { P.list $2 $1 $3 }
listElements
  :                         { [] }
  | expr                    { [$1] }
  | expr ',' listElements   { $1 : $3 }

tuple
  : '(' expr tupleElems ')'    { P.tuple ($2:$3) $1 $4 }
tupleElems
  : ',' expr              { [$2] }
  | ',' expr tupleElems   { $2 : $3 }


 
--CONTROL FLOW

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
  : identifier ':'      %shift  { [$1]}
  | patData identifier          { $1 ++ [$2] }

pattern 
  : identifier                          { P.pattern $ P.Var $1 }
  | term                                { P.pattern $ P.Term $ fmap PE.Literal $1   }
  | patData                             { P.pattern $ P.Tagged $1 }
  | '(' identifier patTupleElems ')'    { P.pattern $ P.Tuple ($2 : $3) }
  | '[' ']'                             { P.pattern $ P.List [] Nothing }
  | '[' patListElems patRest ']'        { P.pattern $ P.List $2 $3 }
  | '{' patRecordKeys patRest '}'       { P.pattern $ P.Record $2 $3 }
  | '(' pattern ')'                     { $2 }

patterns
  : pattern ','        { [$1] }
  | patterns pattern   { $1 ++ [$2] }

cases
  : '|' pattern '->' expr        { [P.matchCase $2 $4] }
  | cases '|' pattern '->' expr  { $1 ++ [P.matchCase $3 $5] }


{
  
runSagaExpr :: String -> Either String (P.ParsedData PE.Expr)
runSagaExpr input = input `P.run` parseSagaExpr

}