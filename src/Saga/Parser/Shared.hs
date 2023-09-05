{-# LANGUAGE MonadComprehensions #-}

module Saga.Parser.Shared where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (nub)

import qualified Saga.Lexer.Lexer           as L
import qualified Saga.Lexer.Tokens          as T
import           Saga.Parser.Expr           as E
import qualified Saga.Parser.Types          as PT


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




-- | Utils

idStr :: E.Expr -> String
idStr (E.Identifier id) = id
idStr e                 = error $ "Not identifier expression:\n" ++ show e

tyIdStr :: PT.TypeExpr -> String
tyIdStr (PT.TIdentifier id) = id
tyIdStr e                  = error $ "Not type identifier expression:\n" ++ show e

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan) 


run :: String -> L.Alex a -> Either String a
run =  L.runAlex . BS.pack

parseError :: L.RangedToken -> L.Alex a
parseError tok = do
  (L.AlexPn _ line column, prev, inStr, _) <- L.alexGetInput
  L.alexError $ "Parse error at\nline " <> show line <> "\ncolumn " <> show column <>
                "\nPrevious character: " <> show prev <>
                "\nCurrent input string: " <> show inStr <>
                "\nToken: " <> show tok
