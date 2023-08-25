module Saga.Utils.Utils where
import           Control.Monad.RWS
import           Control.Monad.ST
import           Data.STRef

fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just b) = Right b
fromMaybe a Nothing  = Left a


-- foo :: ST
tabCount :: ST s (STRef s Integer)
tabCount = newSTRef 0



scoped :: MonadReader r m => m a -> (r -> r) -> m a
scoped m f = do
  let scoped' = local f
  scoped' m

