{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Types where
import           Saga.Language.Typechecker.Zonking.Monad (Effects, Zonk (..),
                                                          Zonking)


import qualified Saga.Language.Syntax.AST                as NT (NodeType (..))


instance Zonk NT.Type where
    type Effects NT.Type es = Zonking es
    zonk = undefined
