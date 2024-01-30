{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Syntax.Elaborated.Kinds where


import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST            hiding (NodeType (..))
import           Saga.Language.Syntax.Elaborated.AST

import           Data.Map                            (Map)


import           Control.Monad                       (forM)
import           Saga.Language.Syntax.Polymorphism   (Polymorphic, Qualified,
                                                      Qualifier)
import           Saga.Language.Typechecker.Variables (Variable)



type Kind = Node Elaborated NT.Kind
data instance Node Elaborated NT.Kind where
    Type        :: Kind
    Kind        :: Kind
    Constraint  :: Kind
    Protocol    :: Kind -> Kind
    Var         :: Variable Kind  -> Kind
    Arrow       :: AST Elaborated NT.Kind -> AST Elaborated NT.Kind   -> Kind
    Application :: AST Elaborated NT.Kind -> [AST Elaborated NT.Kind] -> Kind
    Polymorphic :: Polymorphic Kind -> Kind

deriving instance Show Kind
deriving instance Eq Kind
deriving instance Ord Kind

deriving instance Show (AST Elaborated NT.Kind)
deriving instance Eq (AST Elaborated NT.Kind)
deriving instance Ord (AST Elaborated NT.Kind)

data instance Variable Kind where
    Poly        :: String -> Variable Kind
    Unification :: String -> Variable Kind
    Rigid       :: String -> Variable Kind
deriving instance Show (Variable Kind)
deriving instance Eq (Variable Kind)
deriving instance Ord (Variable Kind)

type instance Qualifier Kind = ()

instance Monad m => Visitor m NT.Kind where
  type Pass NT.Kind = Elaborated
  visit f node      = case node of
    Application cons args -> Application <$> visit' cons <*> forM args visit'
    Arrow in' out         -> Arrow <$> visit' in' <*> visit' out

    k                     -> f k
    where
      f' = map f
      visit' = map $ visit f

      map f (Raw node)           = Raw <$> f node
      map f (Annotated node ann) = Annotated <$> f node <*> pure ann




