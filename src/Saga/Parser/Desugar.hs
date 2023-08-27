module Saga.Parser.Desugar where

import           Debug.Trace   (trace)
import           Unsafe.Coerce (unsafeCoerce)



desugar :: a -> b
desugar a | trace "Desugaring not yet implemented!!" False = undefined
desugar a = unsafeCoerce a
