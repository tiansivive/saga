module Saga.Utils.Utils where
import           Control.Monad.RWS
import           Control.Monad.ST
import           Data.List         (intercalate)
import           Data.Map          (Map, toList)
import           Data.STRef
import           Debug.Trace       (traceM)

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

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

(||>) :: a -> (a -> b) -> b
(||>) a f = f a

infixr 9 |>
infixr 9 ||>



class Pretty a where
  pretty :: a -> String

instance (Show k, Show a) => Pretty (Map k a) where
  pretty = toList |> fmap (\(k, v) -> show k ++ ":  " ++ show v) |> intercalate "\n\t"
