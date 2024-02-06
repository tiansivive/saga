{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Expressions where

import           Control.Monad                                 (forM)
import qualified Data.List                                     as List
import           Data.Maybe                                    (fromMaybe)
import           Effectful                                     (Eff)
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import           Prelude                                       hiding (id,
                                                                lookup)
import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      (Phase (..))
import qualified Saga.Language.Syntax.Elaborated.AST           as AST
import           Saga.Language.Syntax.Elaborated.AST           (AST)
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EK
import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import qualified Saga.Language.Syntax.Elaborated.Values        as EX
import qualified Saga.Language.Syntax.Protocols                as P
import           Saga.Language.Syntax.Protocols                (Protocol (spec),
                                                                ProtocolID, id)

import           Saga.Language.Typechecker.Env                 (CompilerState (..))
import           Saga.Language.Typechecker.Errors              (Exception (..),
                                                                SagaError (..),
                                                                crash)
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Zonking.Monad       (Context (..),
                                                                Zonk (..),
                                                                Zonking)
import           Saga.Utils.Common                             (mapM2)
import           Saga.Utils.Operators                          ((|$>), (|>),
                                                                (||>))

import           Control.Applicative                           ((<|>))
import           Data.Generics.Uniplate.Data                   (transformBiM,
                                                                transformM)
import qualified Data.Map                                      as Map
import           Debug.Pretty.Simple                           (pTrace, pTraceM)
import           Saga.Language.Typechecker.Solving.Monad       (Solution (..))
import           Saga.Language.Typechecker.Solving.Shared      (Tag (..), Var)
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Language.Typechecker.Zonking.Types



zonkE :: (Zonking es) => AST Elaborated NT.Expression -> Eff es (AST Elaborated NT.Expression)
zonkE node | pTrace ("\n---------------------\nZonking\n---------------------\n " ++ show node) False = undefined
zonkE node = do
    z <- transformM subs node
    --pTraceM $ "Zonked\n" ++ show z
    parameterize z

    where
        subs :: Zonking es => AST Elaborated NT.Expression -> Eff es (AST Elaborated NT.Expression)
        subs ast | pTrace ("\n---------------------\nSubstituting\n---------------------\n" ++ show ast) False = undefined
        subs ast@(AST.Annotated (EX.Var (EX.Identifier x)) ann) = do
            ann' <- zonkT ann
            Context { solution, residuals } <- Eff.ask
            let params = collect residuals ||> fmap (\(ev, _, _) -> ev)
            if x `elem` params
                then return $ AST.Annotated (EX.Var $ EX.Identifier x) ann'
                else lookup E (Solver.Evidence x) >>= \case
                    Just (Solver.Protocol (P.Implementation (P.Name id, ty, expr))) -> return $ AST.Annotated (EX.Var $ EX.Identifier id) ann'
                    Just ev -> Eff.throwError $ UnexpectedEvidence ev "Expected Protocol Implementation"

                    Nothing -> return $ AST.Annotated (EX.Var $ EX.Identifier x) ann'

        subs ast@(AST.Annotated expr ann) = do
            ann' <- zonkT ann
            return $ AST.Annotated expr ann'
        subs ast = return ast


        --parameterize expr | pTrace ("Parameterizing: " ++ show expr) False = undefined
        parameterize expr = do
            Context { solution, residuals } <- Eff.ask
            case collect residuals of
                []     -> return expr
                params -> addMissingProtocol params expr

        addMissingProtocol params ast@(AST.Annotated expr ann') = do
                Saga { protocols } <- Eff.ask
                types <- params
                        ||> fmap (\(param, prtcl, t) ->
                            ( prtcl
                            , protocols ||> List.find (\P.Protocol { id } -> id == prtcl)
                                        |$> \P.Protocol { spec } -> do

                                            -- | HACK This needs to evaluate the application of the types, but we don't have that yet
                                            return $ AST.Annotated (ET.Applied (AST.Raw $ ET.Polymorphic spec) (AST.Raw t)) (AST.Raw EK.Kind)))
                        |> mapM2 sequence
                types' <- forM types $ \(prtcl, t) ->
                        let err = Eff.throwError $ MissingProtocol prtcl
                        in maybe err return t

                let annotation = types' ||> foldl (\ty t -> AST.Annotated (ET.Arrow t ty) (AST.Raw EK.Kind)) ann'

                let lambda = EX.Lambda (params ||> fmap (\(id, _, _) -> id)) ast
                return $ AST.Annotated lambda annotation



collect ::  [Solver.Constraint] -> [(String, ProtocolID, ET.Type)]
collect cs = [(ev, pid, t) | Solver.Implementation (Solver.Evidence ev) t pid <- cs]


lookup :: Zonking es => Tag a -> Var a -> Eff es (Maybe a)
lookup T k = Eff.asks $ solution |> tvars |> Map.lookup k
lookup E k = do
    Solution { evidence, witnessed } <- Eff.asks solution
    return $ cycle (evidence, witnessed) 0 k

    where
        cycle (evidence, witnessed) count k
            | count > Map.size evidence = Nothing
            | otherwise                 = Map.lookup k evidence
                                        <|> (Map.lookup k witnessed >>= cycle (evidence, witnessed) (count + 1))


