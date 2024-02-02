{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


module Saga.Language.Typechecker.Elaboration.Types.Shared where
import qualified Data.Map                                            as Map
import           Effectful                                           (Eff, (:>))
import qualified Effectful.Error.Static                              as Eff
import qualified Effectful.Reader.Static                             as Eff
import qualified Effectful.State.Static.Local                        as Eff
import           Saga.Language.Syntax.AST                            (AST,
                                                                      Annotation,
                                                                      NodeType (..),
                                                                      Phase (..))
import qualified Saga.Language.Syntax.Elaborated.AST                 as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds               as EK
import qualified Saga.Language.Syntax.Elaborated.Types               as EL
import           Saga.Language.Syntax.Elaborated.Types               (Node)
import           Saga.Language.Typechecker.Elaboration.Effects       (Elaboration,
                                                                      State (..))
import           Saga.Language.Typechecker.Elaboration.Instantiation
import           Saga.Language.Typechecker.Elaboration.Monad         (Instantiate (..))
import           Saga.Language.Typechecker.Env                       (CompilerState (..))
import           Saga.Language.Typechecker.Errors                    (Exception (NotYetImplemented),
                                                                      SagaError (UnboundVariable),
                                                                      crash)
import qualified Saga.Language.Typechecker.Solving.Constraints       as Solver
import           Saga.Language.Typechecker.Solving.Constraints       (Evidence)
import qualified Saga.Language.Typechecker.Variables                 as Var
import           Saga.Language.Typechecker.Variables                 (Variable)
import           Saga.Utils.Operators                                ((|>))
import           Saga.Utils.TypeLevel                                (type (§))


fresh :: (Elaboration es) => Eff es § Variable EK.Kind
fresh = fresh' K

mkEvidence :: (Elaboration es) => Eff es § Variable Solver.Constraint
mkEvidence = fresh' E



fresh' :: Elaboration es => Tag a -> Eff es (Var a)
fresh' E = do
    i <- Eff.gets $ evars |> (+1)
    Eff.modify $ \s -> s { evars = i}
    let count = show ([1 ..] !! i)
    return $ Solver.Evidence $ "ev" ++ count
fresh' T = do
    i <- Eff.gets $ tvars |> (+1)
    Eff.modify $ \s -> s {tvars  = i}
    let count = show ([1 ..] !! i)
    kvar <- EK.Var <$> fresh' K
    return $ EL.Unification ("t" ++ count) kvar
fresh' K = do
    i <- Eff.gets $ kvars |> (+1)
    Eff.modify $ \s -> s {kvars  = i}
    let count = show ([1 ..] !! i)
    return $ EK.Unification ("k" ++ count)

data Tag a where
  E  :: Tag Evidence
  T  :: Tag EL.Type
  K  :: Tag EK.Kind



type family Var a where
    Var EL.Type  = Variable EL.Type
    Var EK.Kind  = Variable EK.Kind
    Var Evidence = Variable Solver.Constraint






extract :: Show (Node Elaborated e) => AST Elaborated e -> Node Elaborated (Annotation e)
extract = EL.annotation |> EL.node

lookup ::(Elaboration es) => String -> Eff es (AST Elaborated Kind)
lookup x = do
  Saga { kinds, extra } <- Eff.ask

  case Map.lookup x kinds of
    Just (EL.Raw node)           -> EL.Raw <$> walk node
    Just (EL.Annotated node ann) -> crash $ NotYetImplemented "Annotated kinds not yet supported"
    Nothing                      -> Eff.throwError $ UnboundVariable x


  where
    walk (EK.Polymorphic poly) = fresh >>= EK.Var |> instantiate poly |> walk
    walk t                     = return t
