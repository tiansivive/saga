{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}

module Saga.Parser.ParsingInfo where

import qualified Saga.Lexer.Lexer            as L
import qualified Saga.Lexer.Tokens           as T


import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BS

import           Data.Char                   (isLower)
import           Data.List                   (nub)
import           Data.Maybe                  (Maybe (..), fromJust)
import           Data.Monoid                 (First (..))

import           Saga.Lexer.Lexer            (RangedToken (rtToken))

import           Data.Functor                ((<&>))
import           Debug.Trace                 (trace)


import           Control.Monad               (liftM, liftM3)
import           Data.Bitraversable          (bimapM)
import qualified Saga.Language.Core.Literals as PL
import qualified Saga.Parser.Expr            as E
import           Saga.Parser.Shared          (Expandable (..), ParsedData (..),
                                              idStr, tyIdStr)
import qualified Saga.Parser.Types           as PT




-- | EXPRESSIONS
identifier :: L.RangedToken -> (String -> a) -> ParsedData a
identifier rt constructor = Parsed expr (L.rtRange rt) [rt]
  where
    value tok | T.Id name <- rtToken tok = BS.unpack name
    value tok | T.Hole name <- rtToken tok = BS.unpack name
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

keyValPair :: ParsedData E.Expr -> ParsedData a -> ParsedData (String, a)
keyValPair k v = Parsed (id, value v) range' toks
    where
        id = idStr $ value k
        range' = range k <-> range v
        toks = tokens k ++ tokens v

record :: [ParsedData (String, E.Expr)] -> RangedToken -> RangedToken -> ParsedData E.Expr
record pairs start end = Parsed rec' range' toks
    where
        rec' = E.Record $ map value pairs
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] pairs

list :: [ParsedData E.Expr] -> RangedToken -> RangedToken -> ParsedData E.Expr
list elems start end = Parsed list' range' toks
    where
        list' = E.List $ map value elems
        range' = L.rtRange start <-> L.rtRange end
        toks = nub $ foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] elems



tuple :: [ParsedData E.Expr] -> RangedToken -> RangedToken -> ParsedData E.Expr
tuple elems start end =  Parsed tuple' range' toks
    where
        tuple' = E.Tuple $ map value elems
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] elems



term :: ParsedData PL.Literal -> ParsedData E.Expr
term = fmap E.Literal

controlFlow :: L.RangedToken -> ParsedData E.Expr -> ParsedData E.Expr -> ParsedData E.Expr -> ParsedData E.Expr
controlFlow rtStart (Parsed cond _ toks) (Parsed true _ toks') (Parsed false end toks'') = Parsed val span tokens
    where
        val = E.IfElse cond true false
        span = L.rtRange rtStart <-> end
        tokens = toks ++ toks' ++ toks'' ++ [rtStart]


data Pattern a
    = Wildcard a
    | Var a
    | Hole a
    | Term a
    | Tagged a [Pattern a]
    | List [Pattern a] (Maybe a)
    | Tuple [Pattern a]  (Maybe a)
    | Record [(a, Maybe (Pattern a))] (Maybe a)

    deriving (Show, Eq)

pattern :: Pattern (ParsedData E.Expr) -> ParsedData E.Pattern
--pattern e | trace ("\nBuilding pattern: " ++ show e) False = undefined
pattern = \case
    Wildcard e -> fmap (const E.Wildcard) e
    Hole e -> fmap (E.PatHole . idStr) e
    Var e -> fmap (E.Id . idStr) e
    Term e -> fmap (E.Lit . toLit) e
    Tagged tag pats -> E.PatData <$> fmap idStr tag <*> transform pats
    Tuple pats rest -> E.PatTuple <$> transform pats <*> leftover rest
    List pats rest -> E.PatList <$> transform pats <*> leftover rest
    Record pats rest -> E.PatRecord <$> keyValPattern pats <*> leftover rest
    where
        toLit (E.Literal t) = t
        toLit e = error $ "Unexpected non Literal value in Term pattern: " ++ show e
        leftover rest = fmap idStr <$> sequence rest
        transform =  mapM pattern
        keyValPattern :: [(ParsedData E.Expr, Maybe (Pattern (ParsedData E.Expr)))] -> ParsedData [(String, Maybe E.Pattern )]
        keyValPattern = mapM $ bimapM (fmap idStr) $ mapM pattern







matchCase :: ParsedData E.Pattern -> ParsedData E.Expr -> ParsedData E.Case
--matchCase p e | trace ("Building case:\n\tPattern: " ++ show p ++ "\n\tExpr: " ++ show e) False = undefined
matchCase pat expr = [ E.Case p e | p <- pat, e <- expr]


match :: ParsedData E.Expr -> [ParsedData E.Case] -> ParsedData E.Expr
--match e cs | trace ("Building match:\n\tExpr: " ++ show e ++ "\n\tCases: " ++ show cs) False = undefined
match expr cases = [ E.Match e cs | e <- expr, cs <- sequence cases]



parenthesised :: ParsedData E.Expr -> RangedToken -> RangedToken -> ParsedData E.Expr
parenthesised expr start end = expr
    { range = L.rtRange start <-> L.rtRange end
    , tokens = start :end : tokens expr
    }


lambda :: [ParsedData E.Pattern] -> ParsedData E.Expr -> RangedToken -> ParsedData E.Expr
lambda params body rt = E.Lambda <$> sequence params <*> body


fnApplication :: ParsedData E.Expr -> [ParsedData E.Expr] -> ParsedData E.Expr
fnApplication fn args =
    let
        expr = E.FnApp (value fn) $ fmap value args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (tokens fn) args
    in Parsed expr (range fn <-> range (last args)) (nub toks)

bangApplication :: ParsedData E.Expr -> [ParsedData E.Expr] -> RangedToken -> ParsedData E.Expr
bangApplication fn args rt =
    let
        expr = E.FnApp (value fn) $ fmap value args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (rt: tokens fn) args
    in Parsed expr (range fn <-> L.rtRange rt) (nub toks)

infixApplication :: ParsedData E.Expr -> [ParsedData E.Expr] -> ParsedData E.Expr
infixApplication fn args =
    let
        expr = E.FnApp (value fn) $ fmap value args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (tokens fn) args
    in Parsed expr (range fn <-> range (last args)) (nub toks)


binaryOp :: ParsedData E.Expr -> L.RangedToken -> ParsedData E.Expr -> ParsedData E.Expr
--binaryOp l o r | trace ("\nbinaryOp: " ++ show o) False = undefined
binaryOp exprL rtok exprR =
    case L.rtToken rtok of
        T.Operator op -> Parsed (binaryFn $ BS.unpack op) rng toks
        T.Dot         -> Parsed (binaryFn ".") rng toks
        tok           -> error $ "Unrecognised operator token " ++ show tok
    where
        binaryFn fn = E.FnApp (E.Identifier fn) [value exprL, value exprR]
        rng = range exprL <-> range exprR
        toks = nub $ rtok : tokens exprL ++ tokens exprR

dotLambda :: ParsedData E.Expr -> ParsedData E.Expr
dotLambda expr = fmap (const fn) expr
    where
        obj = "_"
        body = E.FnApp (E.Identifier ".") [E.Identifier obj, value expr]
        -- body = PT.FieldAccess (E.Identifier obj) (idStr $ value expr)
        fn = E.Lambda [E.Id obj] body



binding:: ParsedData E.Expr -> ParsedData E.Expr -> ParsedData (E.Binding E.Expr)
binding id expr = Parsed (E.Bind id' expr') range' toks
    where
        extract (Parsed val _ _) = val
        expr' = extract expr
        (E.Identifier id') = extract id
        toks = tokens id ++ tokens expr
        range' = range id <-> range expr

clause:: ParsedData E.Expr -> [ParsedData (E.Binding E.Expr)] -> ParsedData E.Expr
clause (Parsed expr' rt ts) bindings =
    let
        extract (Parsed val _ _) = val
        bindings' = fmap extract bindings
        toks = foldl (\toks' binding -> toks' ++ tokens binding) ts bindings
    in Parsed (E.Clause expr' bindings') (rt <-> range (last bindings)) (nub toks)


-- | BLOCKS

returnStmt :: ParsedData E.Expr -> RangedToken -> ParsedData E.Statement
returnStmt expr tok = Parsed (E.Return $ value expr) rng toks
    where
        rng = L.rtRange tok <-> range expr
        toks = nub $ tok : tokens expr


backcall :: [ParsedData E.Pattern] -> ParsedData E.Expr -> ParsedData E.Statement
backcall pats expr = [ E.BackCall pats' expr'
                     | pats' <- sequence pats
                     , expr' <- expr
                     ]

block :: [ParsedData E.Statement] -> RangedToken -> RangedToken -> ParsedData E.Expr
block stmts _ _ = E.Block <$> sequence stmts






-- | TYPES
tyRecord :: [ParsedData (String, PT.TypeExpr)] -> RangedToken -> RangedToken -> ParsedData PT.TypeExpr
tyRecord pairs start end = Parsed rec' range' toks
    where
        rec' = PT.TERecord $ map value pairs
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] pairs

tyTuple :: [ParsedData PT.TypeExpr] -> RangedToken -> RangedToken -> ParsedData PT.TypeExpr
tyTuple elems start end =  Parsed tuple' range' toks
    where
        tuple' = PT.TETuple $ map value elems
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] elems



tyParenthesised :: ParsedData PT.TypeExpr -> RangedToken -> RangedToken -> ParsedData PT.TypeExpr
tyParenthesised expr@(Parsed val range _) start end = expr
    { value = val
    , range = L.rtRange start <-> L.rtRange end
    }


typeArrow :: ParsedData PT.TypeExpr -> ParsedData PT.TypeExpr ->  ParsedData PT.TypeExpr
typeArrow input output =
    let
        ty = PT.TEArrow (value input) (value output)
        toks = tokens input ++ tokens output
    in Parsed ty (range input <-> range output) (nub toks)

typeLambda :: [ParsedData E.Expr] -> ParsedData PT.TypeExpr -> RangedToken -> ParsedData PT.TypeExpr
typeLambda params (Parsed body' bRange bToks) rt =
    let
        ty = PT.TLambda (fmap (idStr . value) params) body'
        toks = foldl (\toks' param -> toks' ++ tokens param) (rt:bToks) params
    in Parsed ty (L.rtRange rt <-> bRange) (nub toks)

typeFnApplication :: ParsedData PT.TypeExpr -> [ParsedData PT.TypeExpr] -> ParsedData PT.TypeExpr
typeFnApplication fn args = PT.TFnApp <$> fn <*> sequence args


tyIdentifier :: ParsedData E.Expr -> ParsedData PT.TypeExpr
tyIdentifier = fmap $ PT.TIdentifier . idStr


typeUnion :: [ParsedData PT.TypeExpr] -> ParsedData PT.TypeExpr
typeUnion branches = [ PT.TEUnion tyExprs | tyExprs <- sequence branches ]


tagged :: ParsedData E.Expr -> ParsedData PT.TypeExpr  -> ParsedData PT.TypeExpr
tagged expr tyExpr = Parsed tag rng toks
    where
        tag = PT.TTagged (idStr $ value expr) (value tyExpr)
        rng = range expr <-> range tyExpr
        toks = tokens expr ++ tokens tyExpr


data BindingType = Id | Impl | Subtype | Refinement
tyBinding:: BindingType -> ParsedData E.Expr -> ParsedData PT.TypeExpr -> ParsedData (PT.Binding PT.TypeExpr)
tyBinding bindType id expr = case bindType of
    Id         -> Parsed (PT.Bind id' expr') range' toks
    Impl       -> Parsed (PT.ImplBind (PT.TIdentifier id') (tyIdStr expr')) range' toks
    Subtype    -> Parsed (PT.SubtypeBind id' expr') range' toks
    Refinement -> Parsed (PT.RefineBind id' expr') range' toks
    where
        expr' = value expr
        id' = idStr $ value id
        toks = tokens id ++ tokens expr
        range' = range id <-> range expr

typeClause:: ParsedData PT.TypeExpr -> [ParsedData (PT.Binding PT.TypeExpr)] -> ParsedData PT.TypeExpr
typeClause (Parsed expr' rt ts) bindings =
    let
        bindings' = fmap value bindings
        toks = foldl (\toks' binding -> toks' ++ tokens binding) ts bindings
    in Parsed (PT.TClause expr' bindings') (rt <-> range (last bindings)) (nub toks)


typeProtocolImplementation :: ParsedData PT.TypeExpr -> ParsedData PT.TypeExpr -> ParsedData PT.TypeExpr
typeProtocolImplementation protocol tyExpr = PT.TImplementation <$> fmap tyIdStr protocol <*> tyExpr


-- | KINDS
kindArrow :: ParsedData PT.Kind -> ParsedData PT.Kind ->  ParsedData PT.Kind
kindArrow input output =
    let
        kind = PT.KArrow (value input) (value output)
        toks = tokens input ++ tokens output
    in Parsed kind (range input <-> range output) (nub toks)


kindId :: ParsedData E.Expr -> ParsedData PT.Kind
kindId (Parsed e rng toks)
    | idStr e == "Type" = Parsed PT.KType rng toks
    -- | idStr e == "Protocol" = Parsed PT.KProtocol rng toks
    | otherwise =  error $ "Unrecognised Kind: " ++ show e

kindApplication :: ParsedData E.Expr -> ParsedData PT.Kind -> ParsedData PT.Kind
kindApplication id kArg
    | Parsed e _ _ <- id, idStr e == "Protocol" = PT.KProtocol <$> kArg
    | otherwise =  error $ "Unrecognised kind application: " ++ show id


-- | DATA TYPE
dataExpr :: ParsedData E.Expr -> ParsedData PT.TypeExpr -> ParsedData E.DataExpr
dataExpr expr tyExpr = Parsed (id, value tyExpr) range' (nub toks)
    where
        id = idStr $ value expr
        toks = tokens expr ++ tokens tyExpr
        range' = range expr <-> range tyExpr


-- | DECLARATIONS
letdec :: ParsedData E.Expr -> Maybe (ParsedData PT.TypeExpr) -> Maybe (ParsedData PT.Kind) -> ParsedData E.Expr -> ParsedData E.Declaration
--letdec id ty k e | trace ("LetDec:\n\tid: " ++ show id ++ "\n\tType: " ++ show ty ++ "\n\tKind: " ++ show k ++ "\n\tExpression: " ++ show e)  False = undefined
letdec idExpr tyExpr kind expr = Parsed dec (range expr) (tokens expr)
    where
        id = idStr $ value idExpr
        dec = E.Let id (fmap value tyExpr) (fmap value kind) (value expr)

dataType :: ParsedData E.Expr -> Maybe (ParsedData PT.Kind) -> [ParsedData E.DataExpr] -> [ParsedData (PT.Binding PT.TypeExpr)]  -> ParsedData E.Declaration
dataType (Parsed expr rt ts) kind dtExprs bindings = Parsed d range' (nub dataToks)
    where
        id = idStr expr
        dataToks = foldl (\toks' d -> toks' ++ tokens d) ts dtExprs
        -- toks = maybe tokens [] kind  ++ dataToks
        range' = rt <-> range (last dtExprs)
        d = E.Data id (fmap value kind) (fmap value dtExprs) (fmap value bindings)

typeDef :: ParsedData E.Expr -> Maybe (ParsedData PT.Kind) -> ParsedData PT.TypeExpr -> ParsedData E.Declaration
typeDef expr kind tyExpr =
    [ E.Type (idStr e) k ty
    | e <- expr
    , k <- sequence kind
    , ty <- tyExpr
    ]


implementation :: ParsedData E.Expr -> ParsedData PT.TypeExpr -> ParsedData PT.TypeExpr
implementation expr tyExpr = [ PT.TImplementation (idStr e) ty | e <- expr, ty <- tyExpr]


-- | SCRIPTS
script :: [ParsedData E.Declaration] -> ParsedData E.Script
script decs = [ E.Script decs' | decs' <- sequence decs]


