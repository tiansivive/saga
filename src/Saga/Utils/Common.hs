module Saga.Utils.Common where
import           Control.Monad.Reader (MonadReader (local))

fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just b) = Right b
fromMaybe a Nothing  = Left a

scoped :: MonadReader r m => m a -> (r -> r) -> m a
scoped m f = do
  let scoped' = local f
  scoped' m
