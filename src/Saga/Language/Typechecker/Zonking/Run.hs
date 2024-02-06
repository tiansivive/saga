{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ParallelListComp   #-}
{-# LANGUAGE ViewPatterns       #-}
module Saga.Language.Typechecker.Zonking.Run where
import           Control.Monad                                   (forM)
import qualified Data.Set                                        as Set
import           Effectful                                       (Eff)
import qualified Effectful                                       as Eff
import qualified Effectful.Reader.Static                         as Eff


import           Debug.Pretty.Simple                             (pTrace,
                                                                  pTraceM)



import qualified Saga.Language.Syntax.AST                        as NT
import           Saga.Language.Syntax.AST                        (AST,
                                                                  Phase (..))
import qualified Saga.Language.Syntax.Elaborated.Kinds           as ZK
import qualified Saga.Language.Syntax.Elaborated.Types           as ZT
import           Saga.Language.Syntax.Elaborated.Types           (Type)
import qualified Saga.Language.Syntax.Elaborated.Values          as EL
import qualified Saga.Language.Syntax.Elaborated.Values          as Z
import           Saga.Language.Typechecker.Zonking.Monad         (Context (..),
                                                                  Zonking, zonk)
--import           Saga.Language.Typechecker.Zonking.Qualification

import qualified Saga.Language.Syntax.Elaborated.AST             as Z
import qualified Saga.Language.Typechecker.Shared                as Shared

import           Saga.Language.Typechecker.Zonking.Expressions   hiding
                                                                 (collect)
import           Saga.Language.Typechecker.Zonking.Types
import           Saga.Utils.Common                               (fmap2)

import           Data.Bifunctor                                  (Bifunctor (bimap))
import           Data.Data                                       (Data)
import           Data.Generics.Uniplate.Data                     (universeBi)
import qualified Data.List                                       as List
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Set                                        (Set)
import           Prelude                                         hiding
                                                                 (traverse)
import           Saga.Language.Syntax.Polymorphism               (Given (..),
                                                                  Polymorphic (..),
                                                                  Qualified (..))
import qualified Saga.Language.Syntax.Reduced.Types              as Solver
import qualified Saga.Language.Typechecker.Solving.Constraints   as Solver
import           Saga.Language.Typechecker.Substitution          (Substitutable (..))
import           Saga.Language.Typechecker.Variables             (Variable)
import qualified Saga.Language.Typechecker.Zonking.Normalisation as Normalise
import           Saga.Language.Typechecker.Zonking.Normalisation
import           Saga.Language.Typechecker.Zonking.Qualification (qualify)
import           Saga.Utils.Operators                            ((|>), (||>))
import           Saga.Utils.TypeLevel                            (type (§))

run :: forall es. Zonking es => AST Elaborated NT.Expression -> Eff es (AST Elaborated NT.Expression, Type)
run ast  = do
    zonked <- zonkE ast
    Context { residuals } <- Eff.ask
    residuals' <- Eff.runReader (mapping zonked residuals) $ forM residuals (Normalise.traverse @Solver.Constraint)
    ast' <- Eff.runReader (mapping zonked residuals) $ (Normalise.traverse @ZT.Type) zonked >>= (Normalise.traverse @ZK.Kind)
    qt <- qualify ast' residuals'

    return (ast', qt)




mapping :: AST Elaborated NT.Expression -> [Solver.Constraint] -> (Map (Variable ZT.Type) String, Map (Variable ZK.Kind) String)
mapping zonked residuals = pTrace ("\n---------------------\n Mapping:\n" ++ show env) env
    where

        env = collect zonked ||> bimap ((++) ftvs |> make) make

        collect node =  ( [ t | ZT.Var t <- universeBi node ]
                            <> concat [ vars | ZT.Qualified (bs :| cs :=> t) <- universeBi node
                                      , let vars = Set.toList $ ftv bs <> ftv cs
                                      ]
                        , [ k | ZK.Var k <- universeBi node]
                        )
        make (List.nub -> set) = Map.fromList $ zip set Shared.letters

        ftvs = Set.toList $ foldl (\set c ->  set `Set.union` ftv c) Set.empty residuals

