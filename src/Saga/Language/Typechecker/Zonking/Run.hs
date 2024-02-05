{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ParallelListComp   #-}
{-# LANGUAGE ViewPatterns       #-}
module Saga.Language.Typechecker.Zonking.Run where
import           Control.Monad                                 (forM)
import qualified Data.Set                                      as Set
import           Effectful                                     (Eff)
import qualified Effectful                                     as Eff
import qualified Effectful.Reader.Static                       as Eff


import           Debug.Pretty.Simple                           (pTrace, pTraceM)



import qualified Saga.Language.Syntax.AST                      as NT
import           Saga.Language.Syntax.AST                      (AST, Phase (..))
import qualified Saga.Language.Syntax.Elaborated.Values        as EL
import qualified Saga.Language.Syntax.Zonked.Kinds             as ZK
import qualified Saga.Language.Syntax.Zonked.Types             as ZT
import           Saga.Language.Syntax.Zonked.Types             (Type)
import qualified Saga.Language.Syntax.Zonked.Values            as Z
import           Saga.Language.Typechecker.Zonking.Monad       (Context (..),
                                                                Zonking, zonk)
import           Saga.Language.Typechecker.Zonking.Norm        (normK, normT)
--import           Saga.Language.Typechecker.Zonking.Qualification

import qualified Saga.Language.Syntax.Zonked.AST               as Z
import qualified Saga.Language.Typechecker.Shared              as Shared

import           Saga.Language.Typechecker.Zonking.Expressions hiding (collect)
import           Saga.Language.Typechecker.Zonking.Types
import           Saga.Utils.Common                             (fmap2)

import           Data.Bifunctor                                (Bifunctor (bimap))
import           Data.Data                                     (Data)
import           Data.Generics.Uniplate.Data                   (universeBi)
import qualified Data.List                                     as List
import qualified Data.Map                                      as Map
import           Data.Set                                      (Set)
import           Saga.Language.Syntax.Polymorphism             (Polymorphic (..))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Utils.Operators                          ((||>))
import           Saga.Utils.TypeLevel                          (type (§))

run :: Zonking es => AST Elaborated NT.Expression -> Eff es (AST Elaborated NT.Expression)
run ast  = do
    zonked <- zonkE ast
    ast' <- normalise zonked
    -- residuals' <- Eff.runReader (mapping zonked) $ forM residuals normalise

    -- qt <- Eff.inject $ qualify zonked residuals


    return (ast')



        -- Set.toList $ ftv zonked <> ftv residuals
normalise node = Eff.runReader env $ normT node >>= normK
    where
        env = mapping node


mapping zonked =  collect zonked ||> bimap make make
    where
        collect node = unzip $ [ (t, k) | ZT.Var t <- universeBi node | ZK.Var k <- universeBi node]
        make (List.nub -> set) = Map.fromList $ zip set Shared.letters

