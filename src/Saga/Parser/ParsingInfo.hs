{-# LANGUAGE MonadComprehensions #-}

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

import           Saga.Lexer.Lexer                        (RangedToken (rtToken))

import           Data.Functor                            ((<&>))
import           Debug.Trace                             (trace)


data ParseError = ParseError { col:: Int, line:: Int }


class Expandable a where
    (<->) :: a -> a -> a

instance Expandable L.Range where
    (<->) r r' = L.Range (L.start r) (L.stop r')

instance Expandable a => Expandable (ParsedData a) where
    left <-> right = [ left' <-> right' | left' <- left, right' <- right]




data ParsedData a = Parsed { value:: a, range:: L.Range, tokens:: [L.RangedToken] }
    deriving (Eq)

instance Show a => Show (ParsedData a) where
    show (Parsed val _ _ ) = show val

instance Functor ParsedData where
    fmap f (Parsed expr range toks) = Parsed (f expr) range toks

instance Monad ParsedData where
    Parsed val range toks >>= f = Parsed val' (range <-> range') unique
        where
            Parsed val' range' toks' = f val
            unique = nub $ toks ++ toks'
    return = pure
instance MonadFail ParsedData where
    fail = error "Failed ParsedData monadic action"

instance Applicative ParsedData where
    pure a = Parsed a (L.Range (L.AlexPn 0 0 0) (L.AlexPn 0 0 0)) []
    pab <*> pa = do
        f <- pab
        f <$> pa




idStr :: HM.Expr -> String
idStr (HM.Identifier id) = id
idStr e                  = error $ "Not identifier expression:\n" ++ show e

tyIdStr :: HM.TypeExpr -> String
tyIdStr (HM.TIdentifier id) = id
tyIdStr e                  = error $ "Not type identifier expression:\n" ++ show e


-- | EXPRESSIONS
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
        id = idStr $ value k
        range' = range k <-> range v
        toks = tokens k ++ tokens v

record :: [ParsedData (String, HM.Expr)] -> RangedToken -> RangedToken -> ParsedData HM.Expr
record pairs start end = Parsed rec' range' toks
    where
        rec' = HM.Record $ map value pairs
        range' = L.rtRange start <-> L.rtRange end
        toks = foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] pairs

list :: [ParsedData HM.Expr] -> RangedToken -> RangedToken -> ParsedData HM.Expr
list elems start end = Parsed list' range' toks
    where
        list' = HM.List $ map value elems
        range' = L.rtRange start <-> L.rtRange end
        toks = nub $ foldl (\toks' parsed -> tokens parsed ++ toks') [start, end] elems



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


data Pattern a
    = Var a
    | Term a
    | List [a] (Maybe a)
    | Tuple [a]
    | Record [a] (Maybe a)
    | Tagged [a]
    deriving (Show, Eq)

pattern :: Pattern (ParsedData HM.Expr) -> ParsedData HM.Pattern
--pattern e | trace ("\nBuilding pattern: " ++ show e) False = undefined
pattern (Var e)  = fmap (HM.Id . idStr) e
pattern (Term e) = fmap (HM.Literal . toLit) e
    where
        toLit (HM.Term t) = t
        toLit _ = error $ "Unexpected non Literal value in Term pattern: " ++ show e

pattern (Tuple vars) = HM.PatTuple <$> mapM (fmap idStr) vars
pattern (List vars rest) = HM.PatList <$> vars' <*> rest'
  where
        rest' = fmap idStr <$> sequence rest
        vars' =  mapM (fmap idStr) vars
pattern (Record vars rest) = HM.PatRecord <$> vars' <*> rest'
    where
        rest' = fmap idStr <$> sequence rest
        vars' =  mapM (fmap idStr) vars

pattern (Tagged vars) =
    [ HM.PatData (idStr tag) (fmap idStr dat)
    | tag <- head vars
    , dat <- sequence $ tail vars
    ]



matchCase :: ParsedData HM.Pattern -> ParsedData HM.Expr -> ParsedData HM.Case
--matchCase p e | trace ("Building case:\n\tPattern: " ++ show p ++ "\n\tExpr: " ++ show e) False = undefined
matchCase pat expr = [ HM.Case p e | p <- pat, e <- expr]



match :: ParsedData HM.Expr -> [ParsedData HM.Case] -> ParsedData HM.Expr
--match e cs | trace ("Building match:\n\tExpr: " ++ show e ++ "\n\tCases: " ++ show cs) False = undefined
match expr cases = [ HM.Match e cs | e <- expr, cs <- sequence cases]


-- assignment :: ParsedData HM.Expr -> ParsedData HM.Expr -> ParsedData HM.Expr
-- assignment id expr = do
--     HM.Identifier id' <- id
--     expr' <- expr
--     Parsed (HM.Assign id' expr') (range id <-> range expr) (nub $ tokens id ++ tokens expr)


parenthesised :: ParsedData HM.Expr -> RangedToken -> RangedToken -> ParsedData HM.Expr
parenthesised expr start end = expr
    { range = L.rtRange start <-> L.rtRange end
    , tokens = start :end : tokens expr
    }


lambda :: [ParsedData HM.Expr] -> ParsedData HM.Expr -> RangedToken -> ParsedData HM.Expr
lambda params (Parsed body' bRange bToks) rt =
    let
        expr = HM.Lambda (fmap (idStr . value) params) body'
        toks = foldl (\toks' param -> toks' ++ tokens param) (rt:bToks) params
    in Parsed expr (L.rtRange rt <-> bRange) (nub toks)


fnApplication :: ParsedData HM.Expr -> [ParsedData HM.Expr] -> RangedToken -> ParsedData HM.Expr
fnApplication fn args rt =
    let
        expr = HM.FnApp (value fn) $ fmap value args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (rt: tokens fn) args
    in Parsed expr (range fn <-> L.rtRange rt) (nub toks)


binaryOp :: ParsedData HM.Expr -> L.RangedToken -> ParsedData HM.Expr -> ParsedData HM.Expr
--binaryOp l o r | trace ("\nbinaryOp: " ++ show o) False = undefined
binaryOp exprL rtok exprR =
    case L.rtToken rtok of
        T.Operator op -> Parsed (binaryFn $ BS.unpack op) rng toks
        T.Dot         -> Parsed (binaryFn ".") rng toks
        tok           -> error $ "Unrecognised operator token " ++ show tok
    where
        binaryFn fn = HM.FnApp (HM.Identifier fn) [value exprL, value exprR]
        rng = range exprL <-> range exprR
        toks = nub $ rtok : tokens exprL ++ tokens exprR

dotLambda :: ParsedData HM.Expr -> ParsedData HM.Expr
dotLambda expr = fmap (const fn) expr
    where
        obj = "_"
        body = HM.FnApp (HM.Identifier ".") [HM.Identifier obj, value expr]
        -- body = HM.FieldAccess (HM.Identifier obj) (idStr $ value expr)
        fn = HM.Lambda [obj] body



binding:: ParsedData HM.Expr -> ParsedData HM.Expr -> ParsedData (HM.Binding HM.Expr)
binding id expr = Parsed (HM.Bind id' expr') range' toks
    where
        extract (Parsed val _ _) = val
        expr' = extract expr
        (HM.Identifier id') = extract id
        toks = tokens id ++ tokens expr
        range' = range id <-> range expr

clause:: ParsedData HM.Expr -> [ParsedData (HM.Binding HM.Expr)] -> ParsedData HM.Expr
clause (Parsed expr' rt ts) bindings =
    let
        extract (Parsed val _ _) = val
        bindings' = fmap extract bindings
        toks = foldl (\toks' binding -> toks' ++ tokens binding) ts bindings
    in Parsed (HM.Clause expr' bindings') (rt <-> range (last bindings)) (nub toks)


-- | BLOCKS

returnStmt :: ParsedData HM.Expr -> RangedToken -> ParsedData HM.Statement
returnStmt expr tok = Parsed (HM.Return $ value expr) rng toks
    where
        rng = L.rtRange tok <-> range expr
        toks = nub $ tok : tokens expr


backcall :: [ParsedData HM.Pattern] -> ParsedData HM.Expr -> ParsedData HM.Statement
backcall pats expr = [ HM.BackCall pats' expr'
                     | pats' <- sequence pats
                     , expr' <- expr
                     ]

block :: [ParsedData HM.Statement] -> RangedToken -> RangedToken -> ParsedData HM.Expr
block stmts _ _ = HM.Block <$> sequence stmts


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
        ty = HM.TEArrow (value input) (value output)
        toks = tokens input ++ tokens output
    in Parsed ty (range input <-> range output) (nub toks)

typeLambda :: [ParsedData HM.Expr] -> ParsedData HM.TypeExpr -> RangedToken -> ParsedData HM.TypeExpr
typeLambda params (Parsed body' bRange bToks) rt =
    let
        ty = HM.TLambda (fmap (idStr . value) params) body'
        toks = foldl (\toks' param -> toks' ++ tokens param) (rt:bToks) params
    in Parsed ty (L.rtRange rt <-> bRange) (nub toks)

typeFnApplication :: ParsedData HM.TypeExpr -> [ParsedData HM.TypeExpr] -> RangedToken -> ParsedData HM.TypeExpr
typeFnApplication fn args rt =
    let
        ty = HM.TFnApp (value fn) $ fmap value args
        toks = foldl (\toks' arg -> toks' ++ tokens arg) (rt: tokens fn) args
    in Parsed ty (range fn <-> L.rtRange rt) (nub toks)



tyIdentifier :: ParsedData HM.Expr -> ParsedData HM.TypeExpr
tyIdentifier = fmap $ HM.TIdentifier . idStr


typeUnion :: [ParsedData HM.TypeExpr] -> ParsedData HM.TypeExpr
typeUnion branches = [ HM.TEUnion tyExprs | tyExprs <- sequence branches ]


tagged :: ParsedData HM.Expr -> ParsedData HM.TypeExpr  -> ParsedData HM.TypeExpr
tagged expr tyExpr = Parsed tag rng toks
    where
        tag = HM.TTagged (idStr $ value expr) (value tyExpr)
        rng = range expr <-> range tyExpr
        toks = tokens expr ++ tokens tyExpr


data BindingType = Id | Impl | Subtype | Refinement
tyBinding:: BindingType -> ParsedData HM.Expr -> ParsedData HM.TypeExpr -> ParsedData (HM.Binding HM.TypeExpr)
tyBinding bindType id expr = case bindType of
    Id         -> Parsed (HM.Bind id' expr') range' toks
    Impl       -> Parsed (HM.ImplBind id' (tyIdStr expr')) range' toks
    Subtype    -> Parsed (HM.SubtypeBind id' expr') range' toks
    Refinement -> Parsed (HM.RefineBind id' expr') range' toks
    where
        expr' = value expr
        id' = idStr $ value id
        toks = tokens id ++ tokens expr
        range' = range id <-> range expr

typeClause:: ParsedData HM.TypeExpr -> [ParsedData (HM.Binding HM.TypeExpr)] -> ParsedData HM.TypeExpr
typeClause (Parsed expr' rt ts) bindings =
    let
        bindings' = fmap value bindings
        toks = foldl (\toks' binding -> toks' ++ tokens binding) ts bindings
    in Parsed (HM.TClause expr' bindings') (rt <-> range (last bindings)) (nub toks)




-- | KINDS
kindArrow :: ParsedData HM.Kind -> ParsedData HM.Kind ->  ParsedData HM.Kind
kindArrow input output =
    let
        kind = HM.KArrow (value input) (value output)
        toks = tokens input ++ tokens output
    in Parsed kind (range input <-> range output) (nub toks)


kindId :: ParsedData HM.Expr -> ParsedData HM.Kind
kindId (Parsed e rng toks)
    | idStr e == "Type" = Parsed HM.KType rng toks
    | idStr e == "Protocol" = Parsed HM.KProtocol rng toks
    | otherwise =  error $ "Unrecognised Kind: " ++ show e




-- | DATA TYPE
dataExpr :: ParsedData HM.Expr -> ParsedData HM.TypeExpr -> ParsedData HM.DataExpr
dataExpr expr tyExpr = Parsed (id, value tyExpr) range' (nub toks)
    where
        id = idStr $ value expr
        toks = tokens expr ++ tokens tyExpr
        range' = range expr <-> range tyExpr


-- | DECLARATIONS
letdec :: ParsedData HM.Expr -> Maybe (ParsedData HM.TypeExpr) -> Maybe (ParsedData HM.Kind) -> ParsedData HM.Expr -> ParsedData HM.Declaration
--letdec id ty k e | trace ("LetDec:\n\tid: " ++ show id ++ "\n\tType: " ++ show ty ++ "\n\tKind: " ++ show k ++ "\n\tExpression: " ++ show e)  False = undefined
letdec idExpr tyExpr kind expr = Parsed dec (range expr) (tokens expr)
    where
        id = idStr $ value idExpr
        dec = HM.Let id (fmap value tyExpr) (fmap value kind) (value expr)

dataType :: ParsedData HM.Expr -> Maybe (ParsedData HM.Kind) -> [ParsedData HM.DataExpr] -> [ParsedData (HM.Binding HM.TypeExpr)]  -> ParsedData HM.Declaration
dataType (Parsed expr rt ts) kind dtExprs bindings = Parsed d range' (nub dataToks)
    where
        id = idStr expr
        dataToks = foldl (\toks' d -> toks' ++ tokens d) ts dtExprs
        -- toks = maybe tokens [] kind  ++ dataToks
        range' = rt <-> range (last dtExprs)
        d = HM.Data id (fmap value kind) (fmap value dtExprs) (fmap value bindings)

typeDef :: ParsedData HM.Expr -> Maybe (ParsedData HM.Kind) -> ParsedData HM.TypeExpr -> ParsedData HM.Declaration
typeDef expr kind tyExpr =
    [ HM.Type (idStr e) k ty
    | e <- expr
    , k <- sequence kind
    , ty <- tyExpr
    ]


implementation :: ParsedData HM.Expr -> ParsedData HM.TypeExpr -> ParsedData HM.TypeExpr
implementation expr tyExpr = [ HM.TImplementation (idStr e) ty | e <- expr, ty <- tyExpr]


-- | SCRIPTS
script :: [ParsedData HM.Declaration] -> ParsedData HM.Script
script decs = [ HM.Script decs' | decs' <- sequence decs]


-- | Utils
lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)


run :: String -> L.Alex a -> Either String a
run =  L.runAlex . BS.pack

parseError :: L.RangedToken -> L.Alex a
parseError tok = do
  (L.AlexPn _ line column, prev, inStr, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column <>
                " Previous character: " <> show prev <>
                " Current input string: " <> show inStr <>
                " Token: " <> show tok
