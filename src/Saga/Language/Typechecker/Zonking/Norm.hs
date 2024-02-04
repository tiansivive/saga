{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Norm where

import           Control.Monad                       (forM)
import           Data.Bifunctor                      (Bifunctor (bimap))
import           Data.Data                           (Data)
import           Data.Generics.Uniplate.Data         (transformBiM, transformM,
                                                      universeBi)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Effectful                           (Eff, (:>))
import qualified Effectful                           as Eff
import qualified Effectful.Error.Static              as Eff
import qualified Effectful.Reader.Static             as Eff
import qualified Saga.Language.Syntax.AST            as NT (NodeType (..))
import           Saga.Language.Syntax.AST            (Phase (Zonked))
import           Saga.Language.Syntax.Polymorphism   (Polymorphic (..))
import qualified Saga.Language.Syntax.Zonked.AST     as Z
import qualified Saga.Language.Syntax.Zonked.Kinds   as ZK
import qualified Saga.Language.Syntax.Zonked.Types   as ZT
import qualified Saga.Language.Syntax.Zonked.Values  as Z
import           Saga.Language.Typechecker.Errors    (SagaError (UnexpectedVariable))
import qualified Saga.Language.Typechecker.Shared    as Shared
import           Saga.Language.Typechecker.Variables (Variable)
import           Saga.Utils.Common                   (forM2)
import           Saga.Utils.Operators                ((||>))
import           Saga.Utils.TypeLevel                (type (§))




type Normalized es = (Eff.Reader (Mapping ZT.Type, Mapping ZK.Kind) :> es)
type Mapping a = Map (Variable a) String


normT :: (Data from,  Normalized es) => from -> Eff es from
normT = transformBiM norm'
    where
        norm' (ZT.Var v) = do
            (mapping, _) <- Eff.ask @(Mapping ZT.Type, Mapping ZK.Kind)
            let replaced = Map.lookup v mapping ||> fmap (\id -> case v of
                    (ZT.Poly _ k)        ->  ZT.Poly id k
                    (ZT.Local _ k)       ->  ZT.Local id k
                    (ZT.Existential _ k) ->  ZT.Local id k
                    )

                    -- v                   -> Eff.throwError $ UnexpectedVariable v)
            return . ZT.Var $ fromMaybe v replaced
        norm' ty = return ty


normK :: (Data from, Normalized es) => from -> Eff es from
normK = transformBiM norm'
    where
        norm' (ZK.Var v) = do
            (_, mapping) <- Eff.ask @(Mapping ZT.Type, Mapping ZK.Kind)
            let replaced = Map.lookup v mapping ||> fmap (\id -> case v of ZK.Poly k -> ZK.Poly (id ++ "k"))
            return . ZK.Var $ fromMaybe v replaced
        norm' k = return k

