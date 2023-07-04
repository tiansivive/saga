module Saga.Parser.ParsingInfo where

import qualified Saga.Lexer.Lexer                        as L
import qualified Saga.Lexer.Tokens                       as T

import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

import           Data.ByteString.Lazy.Char8              (ByteString)
import qualified Data.ByteString.Lazy.Char8              as BS

import           Data.List                               (nub)
import           Data.Maybe                              (Maybe (..), fromJust)
import           Data.Monoid                             (First (..))
import           Saga.AST.Syntax                         (Expr (FnApp))
import           Saga.Lexer.Lexer                        (RangedToken (rtToken))

data ParseError = ParseError { col:: Int, line:: Int }


-- class (Show a) => ParsingInfo a where
--     (<->) :: a -> a -> a
--     details :: a -> ParseError


(<->) :: L.Range -> L.Range -> L.Range
(<->) r r' = L.Range (L.start r) (L.stop r')

pRange :: ParsedData a -> L.Range
pRange (Parsed _ r _) = r


data ParsedData a = Parsed { value:: a, range:: L.Range, tokens:: [L.RangedToken] }

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
lambda params body rt = let params' = sequence params in do
    body' <- body
    params'' <- params'
    let ps = map (\(HM.Identifier s) -> s) params''
    let e = HM.Lambda ps body'
    Parsed e (L.rtRange rt <-> range body) (nub $ [rt] ++ tokens params' ++ tokens body )


fnApplication :: ParsedData HM.Expr -> [ParsedData HM.Expr] -> RangedToken -> ParsedData HM.Expr
fnApplication fn args rt = let args' = sequence args in do
    fn' <- fn
    args'' <- args'
    let e = HM.FnApp fn' args''
    Parsed e (range fn <-> L.rtRange rt) (nub $ tokens fn ++ tokens args' ++ [rt])





lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

run :: String -> L.Alex a -> Either String a
run =  L.runAlex . BS.pack


