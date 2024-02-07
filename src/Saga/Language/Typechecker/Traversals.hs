{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies     #-}
module Saga.Language.Typechecker.Traversals where



import           Control.Monad.Writer                   (execWriter, tell)
import qualified Data.Map                               as Map
import           Data.Set                               (Set)
import qualified Data.Set                               as Set

import           Control.Monad.Identity                 (Identity (runIdentity))
import           Data.Generics.Uniplate.Data            (transform, universeBi)
import           Debug.Pretty.Simple                    (pTrace)
import           Saga.Language.Syntax.AST               (Node)
import qualified Saga.Language.Syntax.Elaborated.Kinds  as K
import           Saga.Language.Syntax.Elaborated.Kinds  (Kind)
import qualified Saga.Language.Syntax.Elaborated.Types  as T
import           Saga.Language.Syntax.Elaborated.Types  (Type)
import qualified Saga.Language.Syntax.Polymorphism      as T
import           Saga.Language.Syntax.Polymorphism      (Given (..),
                                                         Polymorphic (..),
                                                         Qualified (..))
import           Saga.Language.Typechecker.Substitution (Subst,
                                                         Substitutable (..))
import           Saga.Utils.Operators                   ((||>))
import           Saga.Utils.TypeLevel                   (type (§))


instance Substitutable Type where
    type Target Type = Type

    apply sub = transform apply'
        where
            apply' (T.Var v) = Map.findWithDefault (T.Var v) v sub
            apply' (T.Qualified (bs :| cs :=> ty)) = T.Qualified $ (apply sub <$> bs) :| (apply sub <$> cs) :=> ty
            apply' t         = t

    ftv t = Set.fromList $ [ v | T.Var v <-  universeBi t ]


instance Substitutable T.TypeConstraint where
    type Target T.TypeConstraint = Type

    apply sub (T.Implements ty protocol) = T.Implements (apply sub ty) protocol
    apply sub (T.Refinement bs liquid ty) = T.Refinement bs liquid (apply sub ty)

    ftv (T.Implements ty protocol)  = ftv ty
    ftv (T.Refinement bs liquid ty) = ftv ty



instance Substitutable Kind where
    type Target Kind = Kind

    apply sub k = transform apply' k
        where
            apply' (K.Var v) = Map.findWithDefault (K.Var v) v sub
            apply' k         = k

    ftv k = Set.fromList $ [ v | K.Var v <- universeBi k]


