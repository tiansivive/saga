
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
module Saga.Language.Typechecker.Zonking.Normalisation where

import           Control.Monad                                 (forM)
import           Data.Bifunctor                                (Bifunctor (bimap))
import           Data.Data                                     (Data)
import           Data.Generics.Uniplate.Data                   (transformBiM,
                                                                transformM,
                                                                universeBi)
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (fromMaybe)
import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      (Phase (Elaborated))
import qualified Saga.Language.Syntax.Elaborated.AST           as Z
import qualified Saga.Language.Syntax.Elaborated.Kinds         as ZK
import qualified Saga.Language.Syntax.Elaborated.Types         as ZT
import qualified Saga.Language.Syntax.Elaborated.Values        as Z
import           Saga.Language.Syntax.Polymorphism             (Polymorphic (..))
import           Saga.Language.Typechecker.Errors              (SagaError (UnexpectedVariable))
import qualified Saga.Language.Typechecker.Shared              as Shared
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Utils.Common                             (forM2)
import           Saga.Utils.Operators                          ((|>), (||>))
import           Saga.Utils.TypeLevel                          (type (§))




type Normalized es = (Eff.Reader (Mapping ZT.Type, Mapping ZK.Kind) :> es)
type Mapping a = Map (Variable a) String

class Normalisation node where
    normalise :: (Normalized es) => node -> Eff es node

instance Normalisation (Z.Node Elaborated NT.Type) where
    normalise ty = do
            (mapping, _) <- Eff.ask @(Mapping ZT.Type, Mapping ZK.Kind)
            let replace v = fromMaybe v $
                    Map.lookup v mapping ||> fmap (\id ->
                        case v of
                            ZT.Poly _ k        ->  ZT.Poly id k
                            ZT.Unification _ k ->  ZT.Poly id k
                            ZT.Skolem _ k      ->  ZT.Poly id k
                            ZT.Rigid _ k       ->  ZT.Poly id k
                            ZT.Local _ k       ->  ZT.Local id k
                            ZT.Existential _ k ->  ZT.Local id k
                        )
            return $ case ty of
                ZT.Var v -> ZT.Var $ replace v
                ZT.Polymorphic (Forall tvars t) -> ZT.Polymorphic (Forall (fmap replace tvars) t)
                _ -> ty


instance Normalisation (Z.Node Elaborated NT.Kind) where
    normalise (ZK.Var v) = do
        (_, mapping) <- Eff.ask @(Mapping ZT.Type, Mapping ZK.Kind)
        let replaced = Map.lookup v mapping ||> fmap (\id -> case v of ZK.Poly k -> ZK.Poly (id ++ "k"))
        return . ZK.Var $ fromMaybe v replaced
    normalise k = return k

instance Normalisation Solver.Constraint where
    normalise (Solver.Implementation ev ty pid) = do
        ty' <- normalise ty
        return $ Solver.Implementation ev ty' pid
    normalise (Solver.Refinement scope ty liquid) = do
        ty' <- normalise ty
        return $ Solver.Refinement scope ty' liquid
    normalise k = return k



traverse :: forall node from es. (Normalisation node, Data from, Data node, Normalized es) => from -> Eff es from
traverse =  transformBiM (normalise @node)
