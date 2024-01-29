{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}


module Saga.Language.Typechecker.Elaboration.Types.Shared where
import qualified Data.Map                                             as Map
import           Effectful                                            (Eff,
                                                                       (:>))
import qualified Effectful.Error.Static                               as Eff
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.State.Static.Local                         as Eff
import           Saga.Language.Syntax.AST                             (AST,
                                                                       NodeType (..),
                                                                       Phase (..))
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds                as K
import           Saga.Language.Typechecker.Elaboration.Instantiation
import           Saga.Language.Typechecker.Elaboration.Monad          (Instantiate (..))
import           Saga.Language.Typechecker.Elaboration.Values.Effects (State (..))
import           Saga.Language.Typechecker.Env                        (CompilerState (..))
import           Saga.Language.Typechecker.Errors                     (Exception (NotYetImplemented),
                                                                       SagaError (UnboundVariable),
                                                                       crash)
import qualified Saga.Language.Typechecker.Variables                  as Var
import           Saga.Language.Typechecker.Variables                  (Variable)
import           Saga.Utils.Operators                                 ((|>))
import           Saga.Utils.TypeLevel                                 (type (§))


fresh :: (Eff.Reader Var.Level :> es, Eff.State State :> es) => Eff es § Variable K.Kind
fresh = do
  i <- Eff.gets $ kvars |> (+1)
  Eff.modify $ \s -> s { kvars = i }
  let count = show ([1 ..] !! i)
  return $ K.Unification ("k" ++ count)

lookup ::
  (Eff.Reader (CompilerState Elaborated) :> es, Eff.Reader Var.Level :> es, Eff.State State :> es, Eff.Error SagaError :> es)
  => String -> Eff es (AST Elaborated Kind)
lookup x = do
  Saga { kinds, extra } <- Eff.ask

  case Map.lookup x kinds of
    Just (EL.Raw node)           -> EL.Raw <$> walk node
    Just (EL.Annotated node ann) -> crash $ NotYetImplemented "Annotated kinds not yet supported"
    Nothing                      -> Eff.throwError $ UnboundVariable x


  where
    walk (K.Polymorphic poly) = fresh >>= K.Var |> instantiate poly |> walk
    walk t                    = return t
