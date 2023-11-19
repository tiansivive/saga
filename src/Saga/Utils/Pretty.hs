module Saga.Utils.Pretty where
import           Data.List            (intercalate)
import           Data.Map             (Map, toList)

import           Debug.Trace          (traceM)
import           Saga.Utils.Operators ((|>))

class Pretty a where
  pretty :: a -> String

instance (Show k, Show a) => Pretty (Map k a) where
  pretty = toList |> fmap (\(k, v) -> show k ++ ":  " ++ show v) |> intercalate "\n\t"
