module Saga.Utils.Common where
import           Control.Monad        (liftM2)
import           Control.Monad.Except (MonadError (..), runExcept, runExceptT)
import           Control.Monad.Reader (MonadReader (local))

fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just b) = Right b
fromMaybe a Nothing  = Left a

scoped :: MonadReader r m => m a -> (r -> r) -> m a
scoped m f = do
  let scoped' = local f
  scoped' m



mapM2 :: (Monad m, Traversable t, Traversable t1) => (a -> m b) -> t (t1 a) -> m (t (t1 b))
mapM2 = mapM . mapM

forM2 :: (Monad m, Traversable t, Traversable t1) => t (t1 a) -> (a -> m b) -> m (t (t1 b))
forM2 = flip mapM2

fmap2 :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
fmap2 = fmap . fmap


distribute2 :: Monad m => m a1 -> m a2 -> m (a1, a2)
distribute2 = liftM2 (,)
