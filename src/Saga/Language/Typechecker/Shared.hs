module Saga.Language.Typechecker.Shared where

import           Control.Monad (replicateM)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['α' .. 'ω']
