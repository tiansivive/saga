module Saga.Language.Typechecker.Elaboration.Instantiation where


import qualified Data.Map                                         as Map
import qualified Saga.Language.Syntax.Elaborated.Kinds            as K
import           Saga.Language.Syntax.Elaborated.Kinds            (Kind)
import qualified Saga.Language.Syntax.Elaborated.Types            as T
import           Saga.Language.Syntax.Elaborated.Types            (Type)
import           Saga.Language.Syntax.Polymorphism                (Polymorphic (..))
import           Saga.Language.Typechecker.Elaboration.Monad      (Instantiate (..))

import           Saga.Language.Typechecker.Substitution           (Subst,
                                                                   Substitutable (..),
                                                                   mkSubst)

import           Saga.Language.Typechecker.Elaboration.Traversals


instance Instantiate Type where
    instantiate poly t            = case poly of
        Forall [] t'            -> error $ "Cannot instantiate monomorphic type:\n\t" ++ show t' ++ "\nwith\n\t" ++ show t
        Forall [tvar] t'        -> substitute t' $ mkSubst (tvar, t)
        Forall (tvar:tvars) t'  -> T.Polymorphic . Forall tvars $ substitute t' $ mkSubst (tvar, t)


instance Instantiate Kind where
    instantiate poly k = case poly of
        Forall [] k'            -> error $ "Cannot instantiate monomorphic kind:\n\t" ++ show k' ++ "\nwith\n\t" ++ show k
        Forall [kvar] k'        -> substitute k' $ mkSubst (kvar, k)
        Forall (kvar:kvars) k'  -> K.Polymorphic . Forall kvars $ substitute k' $ mkSubst (kvar, k)


substitute :: Substitutable t => t -> Subst (Target t) -> t
substitute = flip apply


