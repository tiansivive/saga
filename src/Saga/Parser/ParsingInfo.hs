module Saga.Parser.ParsingInfo where

import qualified Saga.Lexer.Lexer                        as L
import qualified Saga.Lexer.Tokens                       as T

import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

import           Data.ByteString.Lazy.Char8              (ByteString)
import qualified Data.ByteString.Lazy.Char8              as BS

import           Data.Char                               (isLower)
import           Data.List                               (nub)
import           Data.Maybe                              (Maybe (..), fromJust)
import           Data.Monoid                             (First (..))
import           Saga.AST.Syntax                         (Expr (FnApp),
                                                          Term (LRecord))
import           Saga.Lexer.Lexer                        (RangedToken (rtToken))

data ParseError = ParseError { col:: Int, line:: Int }


class Expandable a where
    (<->) :: a -> a -> a

instance Expandable L.Range where
    (<->) r r' = L.Range (L.start r) (L.stop r')

-- instance Expandable [T.Token] where
--     (<->) toks toks' = nub $ toks ++ toks'


pRange :: ParsedData a -> L.Range
pRange (Parsed _ r _) = r


data ParsedData a = Parsed { value:: a, range:: L.Range, tokens:: [L.RangedToken] }
    deriving (Eq)

instance Show a => Show (ParsedData a) where
    show (Parsed val _ _ ) = show val

instance Functor ParsedData where
    fmap f (Parsed expr range toks) = Parsed (f expr) range toks

instance Applicative ParsedData where
    pure a = Parsed a (L.Range (L.AlexPn 0 0 0) (L.AlexPn 0 0 0)) []

instance MonadFail ParsedData where
    fail = error "Failed ParsedData monadic action"

instance Monad ParsedData where
    (>>=) (Parsed val range toks) f = Parsed val' (range <-> range') unique
        where
            Parsed val' range' toks' = f val
            unique = nub $ toks ++ toks'


identifier :: L.RangedToken -> (String -> a) -> ParsedData a
identifier rt constructor = Parsed expr (L.rtRange rt) [rt]
  where
    value tok | T.Id name <- rtToken tok = BS.unpack name
    expr = constructor $ value rt


literal :: (T.Token -> a) -> (a -> b) -> RangedToken -> ParsedData b
literal value constructor rt =  Parsed expr (L.rtRange rt) [rt]
    where
        expr = constructor . value . rtToken $ rt


number :: (Int -> a) -> RangedToken -> ParsedData a
number = literal $ \(T.Number num) -> num

string :: (String -> a) -> RangedToken -> ParsedData a
string = literal $ \(T.String s) -> BS.unpack s

boolean :: (Bool -> a) -> RangedToken -> ParsedData a
boolean = literal $ \(T.Boolean b) -> b

keyValPair :: ParsedData HM.Expr -> ParsedData a -> ParsedData (String, a)
keyValPair k v = Parsed (id, value v) range' toks
    where
        HM.Identifier id = value k
        range' = range k <-> range v
        toks = tokens k ++ tokens v

record :: [ParsedData (String, HM.Expr)] -> RangedToken -> RangedToken -> ParsedData HM.Expr
record pairs start end = Parsed rec' range' toks
    where
        rec' = HM.Record $ map value pairs
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] pairs



tuple :: [ParsedData HM.Expr] -> RangedToken -> RangedToken -> ParsedData HM.Expr
tuple elems start end =  Parsed tuple' range' toks
    where
        tuple' = HM.Tuple $ map value elems
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] elems



term :: ParsedData HM.Term -> ParsedData HM.Expr
term = fmap HM.Term

controlFlow :: L.RangedToken -> ParsedData HM.Expr -> ParsedData HM.Expr -> ParsedData HM.Expr -> ParsedData HM.Expr
controlFlow rtStart (Parsed cond _ toks) (Parsed true _ toks') (Parsed false end toks'') = Parsed val span tokens
    where
        val = HM.IfElse cond true false
        span = L.rtRange rtStart <-> end
        tokens = toks ++ toks' ++ toks'' ++ [rtStart]

assignment :: ParsedData HM.Expr -> ParsedData HM.Expr -> ParsedData HM.Expr
assignment id expr = do
    HM.Identifier id' <- id
    expr' <- expr
    Parsed (HM.Assign id' expr') (range id <-> range expr) (nub $ tokens id ++ tokens expr)

parenthesised :: ParsedData HM.Expr -> RangedToken -> RangedToken -> ParsedData HM.Expr
parenthesised expr@(Parsed val range _) start end = expr
    { value = HM.Parens val
    , range = L.rtRange start <-> L.rtRange end
    }


parseError :: L.RangedToken -> L.Alex a
parseError tok = do
  (L.AlexPn _ line column, prev, inStr, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column <>
                " Previous character: " <> show prev <>
                " Current input string: " <> show inStr <>
                " Token: " <> show tok


lambda :: [ParsedData HM.Expr] -> ParsedData HM.Expr -> RangedToken -> ParsedData HM.Expr
lambda params (Parsed body' bRange bToks) rt =
    let
        extract (Parsed (HM.Identifier s) _ _) = s
        expr = HM.Lambda (fmap extract params) body'
        toks = foldl (\toks' param -> toks' ++ tokens param) (rt:bToks) params
    in Parsed expr (L.rtRange rt <-> bRange) (nub toks)


fnApplication :: ParsedData HM.Expr -> [ParsedData HM.Expr] -> RangedToken -> ParsedData HM.Expr
fnApplication fn args rt =
    let
       
        expr = HM.FnApp (extract fn) $ fmap extract args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (rt: tokens fn) args
    in Parsed expr (range fn <-> L.rtRange rt) (nub toks)


binaryOp :: ParsedData HM.Expr -> L.RangedToken -> ParsedData HM.Expr -> ParsedData HM.Expr
binaryOp exprL op exprR = Parsed fn (range exprL <-> range exprR) toks
    where
        fn = HM.FnApp (HM.Identifier "+") [value exprL, value exprR]

        toks = nub $ op : tokens exprL ++ tokens exprR




-- | TYPES


tyRecord :: [ParsedData (String, HM.TypeExpr)] -> RangedToken -> RangedToken -> ParsedData HM.TypeExpr
tyRecord pairs start end = Parsed rec' range' toks
    where
        rec' = HM.TERecord $ map value pairs
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] pairs

tyTuple :: [ParsedData HM.TypeExpr] -> RangedToken -> RangedToken -> ParsedData HM.TypeExpr
tyTuple elems start end =  Parsed tuple' range' toks
    where
        tuple' = HM.TETuple $ map value elems
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] elems



tyParenthesised :: ParsedData HM.TypeExpr -> RangedToken -> RangedToken -> ParsedData HM.TypeExpr
tyParenthesised expr@(Parsed val range _) start end = expr
    { value = HM.TParens val
    , range = L.rtRange start <-> L.rtRange end
    }


typeArrow :: ParsedData HM.TypeExpr -> ParsedData HM.TypeExpr ->  ParsedData HM.TypeExpr
typeArrow input output =
    let
        ty = HM.TEArrow (extract input) (extract output)
        toks = tokens input ++ tokens output
    in Parsed ty (range input <-> range output) (nub toks)

typeLambda :: [ParsedData HM.Expr] -> ParsedData HM.TypeExpr -> RangedToken -> ParsedData HM.TypeExpr
typeLambda params (Parsed body' bRange bToks) rt =
    let
        extract (Parsed (HM.Identifier s) _ _) = s
        ty = HM.TLambda (fmap extract params) body'
        toks = foldl (\toks' param -> toks' ++ tokens param) (rt:bToks) params
    in Parsed ty (L.rtRange rt <-> bRange) (nub toks)

typeFnApplication :: ParsedData HM.TypeExpr -> [ParsedData HM.TypeExpr] -> RangedToken -> ParsedData HM.TypeExpr
typeFnApplication fn args rt =
    let
        ty = HM.TFnApp (extract fn) $ fmap extract args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (rt: tokens fn) args
    in Parsed ty (range fn <-> L.rtRange rt) (nub toks)



tyIdentifier :: ParsedData HM.Expr -> ParsedData HM.TypeExpr
tyIdentifier = fmap $ \(HM.Identifier id) -> HM.TIdentifier id


data BindingType = Id | Impl | Subtype | Refinement
tyBinding:: BindingType -> ParsedData HM.Expr -> ParsedData HM.TypeExpr -> ParsedData HM.Binding
tyBinding bindType id expr = case bindType of
    Id         -> Parsed (HM.Bind id' expr') range' toks
    Impl       -> Parsed (HM.ImplBind id' [expr']) range' toks
    Subtype    -> Parsed (HM.SubtypeBind id' expr') range' toks
    Refinement -> Parsed (HM.RefineBind id' expr') range' toks
    where
        expr' = extract expr
        (HM.Identifier id') = extract id
        toks = tokens id ++ tokens expr
        range' = range id <-> range expr

typeClause:: ParsedData HM.TypeExpr -> [ParsedData HM.Binding] -> ParsedData HM.TypeExpr
typeClause (Parsed expr' rt ts) bindings =
    let
        bindings' = fmap extract bindings
        toks = foldl (\toks' binding -> toks' ++ tokens binding) ts bindings
    in Parsed (HM.TClause expr' bindings') (rt <-> range (last bindings)) (nub toks)




-- | KINDS
kindArrow :: ParsedData HM.Kind -> ParsedData HM.Kind ->  ParsedData HM.Kind
kindArrow input output =
    let
        kind = HM.KArrow (extract input) (extract output)
        toks = tokens input ++ tokens output
    in Parsed kind (range input <-> range output) (nub toks)


kindId :: ParsedData HM.Expr -> ParsedData HM.Kind
kindId p
    | Parsed e _ _ <- p
    , HM.Identifier id <- e
    , id == "Type" = fmap (\(HM.Identifier id) -> HM.KType) p

    | otherwise = error "Unrecognised Kind"


-- | DATA TYPE

type DataExpr = (String, HM.TypeExpr)

dataExpr :: ParsedData HM.Expr -> ParsedData HM.TypeExpr -> ParsedData DataExpr
dataExpr expr tyExpr = Parsed (id, extract tyExpr) range' (nub toks)
    where
        HM.Identifier id = extract expr
        toks = tokens expr ++ tokens tyExpr
        range' = range expr <-> range tyExpr
        

-- | DECLARATIONS

data Declaration
    = Let String (Maybe HM.TypeExpr) (Maybe HM.Kind) HM.Expr
    | Type String (Maybe HM.Kind) HM.TypeExpr
    | Data String (Maybe HM.Kind) [DataExpr]

letdec :: ParsedData HM.Expr -> Maybe (ParsedData HM.TypeExpr) -> Maybe (ParsedData HM.Kind) -> ParsedData HM.Expr -> ParsedData Declaration
letdec idExpr tyExpr kind expr = Parsed dec (range expr)(tokens expr)
    where
        HM.Identifier id = extract expr
        dec = Let id (fmap extract tyExpr) (fmap extract kind) (extract expr)

dataType :: ParsedData HM.Expr -> Maybe (ParsedData HM.Kind) -> [ParsedData DataExpr] -> ParsedData Declaration
dataType (Parsed expr rt ts) kind dtExprs = Parsed d range' (nub dataToks)
    where 
        HM.Identifier id = expr
        dataToks = foldl (\toks' d -> toks' ++ tokens d) ts dtExprs
        -- toks = maybe tokens [] kind  ++ dataToks
        range' = (rt <-> range (last dtExprs))
        d = Data id (fmap extract kind) (fmap extract dtExprs)

typeDef :: ParsedData HM.Expr -> Maybe (ParsedData HM.Kind) -> ParsedData HM.TypeExpr -> ParsedData Declaration
typeDef expr kind tyExpr = Parsed tyDef (range expr) (tokens expr)
    where
        HM.Identifier id = extract expr
        tyDef = Type id (fmap extract kind) (extract tyExpr)


-- | SCRIPTS

data Script = Script [Declaration]

script :: [ParsedData Declaration] -> ParsedData Script
script decs = Parsed script' rng toks
    where
        script' = Script $ fmap extract decs
        rng = range $ last decs
        toks = tokens $ last decs

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

run :: String -> L.Alex a -> Either String a
run =  L.runAlex . BS.pack


extract :: ParsedData a -> a
extract (Parsed val _ _) = val

