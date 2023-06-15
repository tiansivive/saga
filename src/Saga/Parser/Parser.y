{
module Saga.Parser.Parser  
    ( runSagaScript
    , runSagaExpr
    , runSagaType
    , runSagaKind
    , runSagaDec
    , parseSagaExpr
    , parseSagaType
    , parseSagaKind
    , parseSagaDec
    ) where

import Data.Char (isLower)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe  (Maybe (..), fromJust)
import Data.Monoid (First (..))
import Data.List (last, head)

import qualified Saga.Lexer.Lexer as L
import qualified Saga.Lexer.Tokens as T
import qualified Saga.AST.Syntax as Syntax
import qualified Saga.AST.TypeSystem.Types as Types
import qualified Saga.AST.TypeSystem.Kinds as Kinds

import qualified Saga.AST.Scripts as Scripts

}

%name parseSagaScript script
%name parseSagaExpr expr
%name parseSagaType typeExpr
%name parseSagaKind kindExpr
%name parseSagaDec dec
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
  : id { unTok $1 (\range (T.Id name) -> Syntax.Name range (BS.unpack name)) }

path
  : identifier          { [$1] }
  | path '.' identifier { $1 ++ [$3]}

 -- COLLECTIONS
pairs
  :                                 { [] }
  | identifier ':' expr ',' pairs   { ($1, $3) : $5 }
  | identifier ':' expr             { [($1, $3)] }

record 
  : '{' pairs '}'   { Syntax.LRecord (L.rtRange $1 <-> L.rtRange $3) $2 }

listElements
  :                         { [] }
  | expr                    { [$1] }
  | expr ',' listElements   { $1 : $3 }

list 
  : '[' listElements ']'    { Syntax.LList (L.rtRange $1 <-> L.rtRange $3) $2 }

tupleElems
  : ',' expr                  { [$2] }
  | ',' expr tupleElems   { $2 : $3 }

tuple
  : '(' expr tupleElems ')'    { Syntax.LTuple (L.rtRange $1 <-> L.rtRange $4) ($2:$3) }


-- FUNCTIONS
args
  :                  { [] }
  | args identifier  { $1 ++ [$2] }

params
  :               { [] }
  | params atom   { $1 ++ [$2] }

fnApplication 
  : atom params '!' { Syntax.FnApp (info $1 <-> L.rtRange $3) $1 $2 }
  -- : atom { [$1] }
  -- | fnApplication atom { $1 ++ [$2] }
  
--CONTROL FLOW
controlFlow 
  : if expr then expr else expr { Syntax.IfElse (L.rtRange $1 <-> info $6) $2 $4 $6 }


-- BLOCKS
block
  : return expr   { [Syntax.Return (L.rtRange $1 <-> info $2) $2] }
  | expr block  { $1 : $2 }


--EXPRESSIONS
term 
  : number     { unTok $1 (\range (T.Number int) -> Syntax.LInt range int) }
  | string     { unTok $1 (\range (T.String string) -> Syntax.LString range string) }
  | boolean    { unTok $1 (\range (T.Boolean boolean) -> Syntax.LBool range boolean) }
  | tuple      { $1 }
  | list       { $1 }
  | record     { $1 }
  
atom
  : identifier              { Syntax.Identifier $1 }
  | atom '.' path           { Syntax.FieldAccess (info $1 <-> (info $ last $3)) $1 $3 }
  | term                    { Syntax.Term $1 }
  | '{' block '}'           { Syntax.Block (L.rtRange  $1 <-> L.rtRange $3) $2 }
  | '(' expr ')'            { Syntax.Parens (L.rtRange  $1 <-> L.rtRange $3) $2 }


assignment 
  : identifier '=' expr     { Syntax.Assign $1 $3 }  

assignments
  : assignment { [$1] }         
  | assignments ',' assignment { $1 ++ [$3] }

expr
  
  : controlFlow             { $1 }
  | fnApplication           { $1 }--{ Syntax.FnApp (info $ head $1 <-> info $ last $1) (head $1) (tail $1) }
  | '\\' args '->' expr     { Syntax.Lambda (L.rtRange $1 <-> info $4) $2 $4 }
  | with assignments in expr { Syntax.Clause (L.rtRange $1 <-> info $4) $2 $4 }
  | atom %shift             { $1 }
  | identifier '=' expr     { Syntax.Assign $1 $3 }  

 

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
  : '{' tpairs '}'   { Types.TRecord (L.rtRange $1 <-> L.rtRange $3) $2 }

ttupleElems
  : ',' typeExpr                  { [$2] }
  | ',' typeExpr ttupleElems   { $2 : $3 }

ttuple
  : '(' typeExpr ttupleElems ')'    { Types.TTuple (L.rtRange $1 <-> L.rtRange $4) ($2:$3) }

typeParams
  : typeAtom { [$1] }
  | typeParams typeAtom { $1 ++ [$2] }


type 
  : number { unTok $1 (\range (T.Number int) -> Types.TLiteral $ Syntax.LInt range int) }
  | string     { unTok $1 (\range (T.String string) -> Types.TLiteral $ Syntax.LString range string) }
  | boolean    { unTok $1 (\range (T.Boolean boolean) -> Types.TLiteral $ Syntax.LBool range boolean) }
  | ttuple      { $1 }
  | trecord     { $1 }
  | identifier %shift  { resolveIdType $1 }
  -- | identifier '<' typeParams '>' { Types.TParametric (Types.Type $ resolveIdType $1) $3 }

 

typeAtom
  : type  { Types.Type $1 } 
  | '(' typeExpr ')' { Types.TParens (L.rtRange  $1 <-> L.rtRange $3) $2 }
  

typeFnArgs
  :                       { [] }
  | typeFnArgs typeAtom   { $1 ++ [$2] }

typeExpr 
  : if typeExpr then typeExpr else typeExpr { Types.TConditional (L.rtRange $1 <-> info $6) $2 $4 $6 }
  | typeExpr '->' typeExpr  { Types.Type $ Types.TArrow (info $1 <-> info $3) $1 $3 }
  | '\\' args '->' typeExpr { Types.TLambda (L.rtRange $1 <-> info $4) $2 $4 }
  | typeAtom typeFnArgs '!' { Types.TFnApp (info $1 <-> L.rtRange $3) $1 $2 }
  | typeAtom     { $1 }
  | qualifiers '.' typeExpr                          { Types.Type $ Types.TConstrained $1 [] $3 }
  | qualifiers '.' constraints '=>' typeExpr %shift  { Types.Type $ Types.TConstrained $1 $3 $5 }
  | implements identifier ':' typeExpr %shift { Types.Type (Types.TImplementation $2 $4 []) }
  | with args '=>' implements identifier ':' typeExpr %shift { Types.Type (Types.TImplementation $5 $7 $2) }

qualifier
  : forall args { fmap (\a -> Types.TPolyVar Types.Forall Types.None a ) $2  }
  | exists args { fmap (\a -> Types.TPolyVar Types.Exists Types.None a ) $2  }

qualifiers
  : qualifier                  { $1 }
  | qualifiers ',' qualifier   { $1 ++ $3 }
  | '(' qualifiers ')'         { $2 }


constraint
  : identifier identifier %shift            { Types.Implements (Types.Type $ resolveIdType $1) (Types.Type $ resolveIdType $2) }
  | identifier implements identifier { Types.Implements (Types.Type $ resolveIdType $3) (Types.Type $ resolveIdType $1) }
  | identifier '|->' typeAtom         { Types.Extends $3 (Types.Type $ resolveIdType $1) }

constraints
  : constraint                  { [$1] }
  | constraints ',' constraint  { $1 ++ [$3]}
  | '(' constraints ')'         { $2 }

typeAnnotation
  : { Nothing }
  | ':' typeExpr { Just $2 }


-- Kinds

kindExpr
  : kindExpr '->' kindExpr  { Kinds.KConstructor $1 $3 }
  | identifier { resolveIdKind $1 }



kindAnnotation
  : { Nothing }
  | '::' kindExpr { Just $2 }

-- SCRIPT


dataExpr
  : identifier ':' typeExpr { ($1, $3) }

dataExprs
    : dataExpr { [$1] }
    | dataExprs '|' dataExpr { $1 ++ [$3] }



dec 
  : let identifier typeAnnotation kindAnnotation '=' expr { Scripts.Let $2 $3 $4 $6 }
  | data identifier kindAnnotation '=' dataExprs          { Scripts.Data $2 $3 $5 }
  | ty identifier kindAnnotation '=' typeExpr             { Scripts.Type $2 $3 $5 }
 

declarations
  : dec              { [$1] }
  | declarations dec { $1 ++ [$2] }

moduleDef
  : module path where { Scripts.Mod (L.rtRange $1 <-> L.rtRange $3) ( map (\(Syntax.Name _ name) -> name) $2 ) } -- TODO: extract to named fn

importMod
  : import path { Scripts.Import (L.rtRange $1 <-> (info $ last $2)) ( map (\(Syntax.Name _ name) -> name) $2 ) }

imports
  :                    { [] }
  | importMod imports  { $1 : $2 }
 
script
  : moduleDef declarations imports { Scripts.Script (info $1 <-> (info $ last $2)) $1 $2 $3 }


{


resolveIdKind :: Syntax.Name a -> Kinds.Kind a
resolveIdKind (Syntax.Name _ id) = 
  case isLower $ head id of
    True  -> Kinds.KVar id
    False -> case id of 
      "Type" -> Kinds.KType
      "Protocol" -> Kinds.KProtocol
      "Constraint" -> Kinds.KConstraint
      k -> error $ "Unrecognised kind identifier:" <> k

resolveIdType :: Syntax.Name a -> Types.Type a
resolveIdType name@(Syntax.Name info id) = 
  case isLower $ head id of
    True  -> Types.TVar name
    False -> case id of 
      "Int"    -> Types.TPrimitive info Types.TInt
      "Bool"   -> Types.TPrimitive info Types.TBool
      "String" -> Types.TPrimitive info Types.TString
      "List"   -> Types.TIdentifier name
      _        -> Types.TIdentifier name



-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok range) contructor = contructor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure


binaryOp :: Syntax.Expr L.Range -> L.RangedToken -> Syntax.Expr L.Range -> Syntax.Expr L.Range 
binaryOp expr1 op expr2 = Syntax.FnApp (info expr1 <-> info expr2) (unTok op (\range (T.Operator char) -> Syntax.Identifier (Syntax.Name range (BS.unpack char)))) [expr1, expr2]


-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2



parseError :: L.RangedToken -> L.Alex a
parseError tok = do
  (L.AlexPn _ line column, prev, inStr, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column <>
                " Previous character: " <> show prev <>
                " Current input string: " <> show inStr <>
                " Token: " <> show tok

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)


run :: String -> L.Alex a -> Either String a
run =  L.runAlex . BS.pack


runSagaScript :: String -> Either String (Scripts.Script L.Range)
runSagaScript input = input `run` parseSagaScript

runSagaExpr :: String -> Either String (Syntax.Expr L.Range)
runSagaExpr input = input `run` parseSagaExpr

runSagaType :: String -> Either String (Types.TypeExpr L.Range)
runSagaType input = input `run` parseSagaType

runSagaKind :: String -> Either String (Kinds.Kind L.Range)
runSagaKind input = input `run` parseSagaKind

runSagaDec :: String -> Either String (Scripts.Declaration L.Range)
runSagaDec input = input `run` parseSagaDec

}