module Saga.Utils.Utils where
import           Control.Monad.ST
import           Data.STRef

fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just b) = Right b
fromMaybe a Nothing  = Left a




-- foo :: ST
tabCount :: ST s (STRef s Integer)
tabCount = newSTRef 0



