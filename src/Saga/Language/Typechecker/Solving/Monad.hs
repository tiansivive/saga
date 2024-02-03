{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Solving.Monad where



import qualified Data.Map                                      as Map
import qualified Effectful.State.Static.Local                  as Eff

import           Saga.Language.Typechecker.Errors              (SagaError)
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import           Saga.Language.Typechecker.Solving.Constraints (Constraint,
                                                                Evidence,
                                                                Variable)

import           Saga.Language.Typechecker.Substitution        (Subst,
                                                                Substitutable,
                                                                compose)

import           Debug.Pretty.Simple                           (pTraceM)
import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import qualified Effectful.Reader.Static                       as Eff


import qualified Saga.Language.Typechecker.Monad               as TC
import           Saga.Language.Typechecker.Solving.Cycles      (Cycle)

import           Saga.Language.Syntax.Elaborated.Types         (Type)
import           Saga.Language.Syntax.Literals
import qualified Saga.Language.Typechecker.Variables           as Var

import           Data.Map                                      (Map)
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Syntax.AST                      hiding
                                                               (NodeType (..))
import           Saga.Language.Syntax.Elaborated.Kinds         (Kind)
import           Saga.Language.Typechecker.Env                 (CompilerState,
                                                                Info)
import           Saga.Language.Typechecker.Traversals


type Solving es =   ( Eff.Reader (CompilerState Elaborated) :> es
                    , Eff.Writer Info :> es
                    , Eff.Error SagaError :> es
                    , Eff.Fail :> es
                    , Eff.Reader Var.Level :> es
                    , Eff.Reader Levels :> es
                    , Eff.State Solution :> es
                    , Eff.State Count :> es
                    , Eff.State [Cycle Type] :> es
                    , Eff.IOE :> es
                    )

data Count = Count { evs :: Int, tvs :: Int, kvs :: Int }
  deriving Show

data Solution = Solution { evidence :: Witness, tvars :: Subst Type, kvars :: Subst Kind, witnessed :: Witnessed, proofs :: Proofs }
  deriving (Show)
data Status = Solved | Deferred | Impossible deriving Show

type Witness = Map (Variable Constraint) Evidence
type Witnessed = Map (Variable Constraint) (Variable Constraint)

type Proofs = Map (Variable Type) Literal
type Levels = Map (Variable Type) Var.Level

class Solve c where
    solve       :: Solving es => c -> Eff es (Status, Constraint)
    simplify    :: Solving es => c -> Eff es Constraint

    irreducible :: c -> Bool
    irreducible = const True

-- | ISSUE #23
-- | TODO #24 Move Constraint list to a state effect within the Solver monad
class Entails a where
    entails :: Solving es => a -> [Constraint] -> Eff es [Constraint]





initialSolution :: Solution
initialSolution = Solution { evidence = Map.empty, tvars = Map.empty, kvars = Map.empty, witnessed = Map.empty, proofs = Map.empty }
initialCount :: Count
initialCount = Count 0 0 0




run :: Eff (Eff.State [Cycle Type] : Eff.State Solution : Eff.State Count : es) a -> Eff es (((a, [Cycle Type]), Solution), Count)
run =  Eff.runState initialCount . Eff.runState initialSolution . Eff.runState []
