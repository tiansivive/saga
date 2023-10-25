module Saga.Language.Typechecker.Kind where


data Kind
  = Type
  | Var String
  | Arrow Kind Kind
  | Protocol Kind
  | Constraint
  | Data String Kind 
    deriving (Show, Eq, Ord)
