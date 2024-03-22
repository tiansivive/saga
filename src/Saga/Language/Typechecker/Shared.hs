module Saga.Language.Typechecker.Shared where

import           Control.Monad (replicateM)

letters :: [String]
letters = [0 ..] >>= flip replicateM ['α' .. 'ω']
