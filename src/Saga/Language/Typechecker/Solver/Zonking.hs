{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Solver.Zonking where
import           Saga.Language.Core.Expr                      (Expr (..))
import           Saga.Language.Typechecker.Solver.Monad       (Ev, Of,
                                                               Solution (..),
                                                               SolverEff,
                                                               SolverM,
                                                               Tag (..), Ty)

import qualified Data.Map                                     as Map
import qualified Effectful.State.Static.Local                 as Eff
import           Saga.Utils.Operators                         ((|>), (||>))

import           Control.Applicative                          ((<|>))
import qualified Data.List                                    as List
import qualified Effectful.Error.Static                       as Eff
import qualified Effectful.Reader.Static                      as Eff
import           Prelude                                      hiding (lookup)
import qualified Saga.Language.Typechecker.Solver.Constraints as C
import           Saga.Language.Typechecker.Solver.Constraints (Constraint,
                                                               Evidence)
import           Saga.Language.Typechecker.Type               (Type)
import qualified Saga.Language.Typechecker.Variables          as Var
import           Saga.Language.Typechecker.Variables          (PolymorphicVar)

type Zonking = SolverEff '[Eff.Reader [ScopedVar]]
type ScopedVar = String

type family KeyFor a where
    KeyFor Ev = PolymorphicVar Evidence
    KeyFor Ty = PolymorphicVar Type





lookup :: Tag a -> KeyFor a -> Zonking (Maybe (Of a))
lookup T k = Eff.gets $ tvars |> Map.lookup k
lookup E k = do
    Solution { evidence, witnessed } <- Eff.get
    return $ cycle (evidence, witnessed) 0 k

    where
        cycle (evidence, witnessed) count k
            | count > Map.size evidence = Nothing
            | otherwise                 = Map.lookup k evidence
                                        <|> (Map.lookup k witnessed >>= cycle (evidence, witnessed) (count + 1))


zonk :: Expr -> [Constraint] -> Zonking Expr
zonk ast residuals = do
    sol@Solution { tvars, evidence, witnessed } <- Eff.get

    -- case ast of
    --     --Identifier x -> maybe x () $ Map.lookup
    Lambda implementations <$> zonk' ast

    where
        implementations = residuals ||> List.filter (\C.Impl {} -> True) |> fmap (\(C.Impl (Var.Evidence id) _ _) -> id)

        zonk' ast = do
            sol@Solution { tvars, evidence, witnessed } <- Eff.get

            case ast of
                Identifier x -> do
                    vars <- Eff.ask
                    if x `elem` vars then
                        return x
                    else do
                        res <- lookup E (Var.Evidence x)
                        _r

            return ast
