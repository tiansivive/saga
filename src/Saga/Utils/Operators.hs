module Saga.Utils.Operators where
import           Control.Monad.RWS
import           Control.Monad.ST
import           Data.List         (intercalate)
import           Data.Map          (Map, toList)
import           Data.STRef
import           Debug.Trace       (traceM)



(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

(||>) :: a -> (a -> b) -> b
(||>) a f = f a

infixr 9 |>
infixr 9 ||>



