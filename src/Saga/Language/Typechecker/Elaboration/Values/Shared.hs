{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Saga.Language.Typechecker.Elaboration.Values.Shared where



import           Effectful                                            (Eff,
                                                                       (:>))
import qualified Effectful.Reader.Static                              as Eff
import qualified Effectful.State.Static.Local                         as Eff
import qualified Effectful.Writer.Static.Local                        as Eff
import           Saga.Language.Typechecker.Elaboration.Values.Effects (State (..))
import qualified Saga.Language.Typechecker.Variables                  as Var

import qualified Data.Map                                             as Map

import qualified Saga.Language.Syntax.AST                             as NT (NodeType (..))
import           Saga.Language.Syntax.AST
import qualified Saga.Language.Syntax.Elaborated.AST                  as EL

import qualified Saga.Language.Syntax.Elaborated.Kinds                as K
import qualified Saga.Language.Syntax.Elaborated.Types                as T
import           Saga.Language.Syntax.Elaborated.Types                (Type (..))
import qualified Saga.Language.Typechecker.Solving.Constraints        as CST
import           Saga.Language.Typechecker.Solving.Constraints        (Evidence)
import           Saga.Language.Typechecker.Variables                  (Variable)
import           Saga.Utils.Operators                                 ((|>))
import           Saga.Utils.TypeLevel                                 (type (§))


type Constructor = String -> K.Kind -> Variable Type
fresh :: (Eff.Reader Var.Level :> es, Eff.State State :> es) => Constructor -> Eff es § Variable Type
fresh constructor = do
  iType <- Eff.gets $ tvars |> (+1)
  Eff.modify $ \s -> s { tvars = iType }
  let typeCount = show ([1 ..] !! iType)

  iKind <- Eff.gets $ kvars |> (+1)
  Eff.modify $ \s -> s { kvars = iKind }
  let kindCount = show ([1 ..] !! iKind)
  let kvar = K.Var $ K.Unification ("k" ++ kindCount)
  let tvar = constructor ("t" ++ typeCount) kvar
  lvl <- Eff.ask @Var.Level
  Eff.modify $ \s -> s { levels = Map.insert tvar lvl $ levels s }
  return tvar


freshK :: (Eff.Reader Var.Level :> es, Eff.State State :> es) => Eff es § Variable K.Kind
freshK = do
  i <- Eff.gets $ kvars |> (+1)
  Eff.modify $ \s -> s { kvars = i }
  let count = show ([1 ..] !! i)
  return $ K.Unification ("k" ++ count)



mkEvidence :: (Eff.State State :> es) => Eff es § Variable CST.Evidence
mkEvidence = do
  i <- Eff.gets $ evars |> (+1)
  Eff.modify $ \s -> s { evars = i }
  let count = show ([1 ..] !! i)
  return $ CST.Evidence ("ev_" ++ count)




toItem :: Type -> CST.Item
toItem (T.Var tvar) = CST.Var tvar
toItem t            = CST.Mono t



extract :: Show (Node 'Elaborated e) => AST 'Elaborated e -> Node 'Elaborated (Annotation e)
extract = EL.annotation |> EL.node



-- QUESTION: Do singletons have a different kind?
decorate ty@(T.Singleton {})              = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Tuple {})                  = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Record {})                 = EL.Annotated ty $ EL.Raw K.Type

decorate ty@(T.Var (T.Unification t k))   = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Instantiation t k)) = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Poly t k))          = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Local t k))         = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Rigid t k))         = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Scoped t k))        = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Skolem t k))        = EL.Annotated ty $ EL.Raw k

