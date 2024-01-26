module Saga.Utils.Operators where
import           Control.Monad.RWS
import           Control.Monad.ST
import           Data.List         (intercalate)
import           Data.Map          (Map, toList)
import           Data.STRef
import           Debug.Trace       (traceM)



(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

(|$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(|$>) = flip ((.) . fmap)

(||>) :: a -> (a -> b) -> b
(||>) a f = f a

(<|:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(<|:) =  (.) . (.)
(|>:) :: (a1 -> a2 -> b) -> (b -> c) -> a1 -> a2 -> c
(|>:) = flip (<|:)

infixr 9 |>
infixr 9 ||>

infixr 9 |$>

infixr 9 |>:
infixr 9 <|:



