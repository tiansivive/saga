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
import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import qualified Saga.Language.Syntax.Elaborated.Values        as EX
import qualified Saga.Language.Syntax.Protocols                as P
import           Saga.Language.Syntax.Protocols                (Protocol (spec),
                                                                ProtocolID, id)
import qualified Saga.Language.Syntax.Zonked.AST               as Z
import qualified Saga.Language.Syntax.Zonked.Kinds             as ZK
import qualified Saga.Language.Syntax.Zonked.Types             as ZT
import qualified Saga.Language.Syntax.Zonked.Values            as Z
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
import qualified Data.Map                                      as Map
import           Saga.Language.Typechecker.Solving.Monad       (Solution (..))
import           Saga.Language.Typechecker.Solving.Shared      (Tag (..), Var)
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Language.Typechecker.Zonking.Types


instance Zonk NT.Expression where
    type Effects NT.Expression es = Zonking es

    zonk node = case node of
        AST.Raw expr -> Z.Raw <$> zonk' expr
        AST.Annotated expr ann -> do
            Context { solution, residuals } <- Eff.ask
            ann' <- zonk ann
            case collect residuals of
                []     -> Z.Annotated <$> zonk' expr <*> pure ann'
                params -> addMissingProtocol params expr ann'


        where
            zonk' :: Zonking es => Z.Node Elaborated NT.Expression -> Eff es (Z.Node Zonked NT.Expression)
            zonk' ast = do
                Context { solution, residuals } <- Eff.ask
                let params = collect residuals ||> fmap (\(ev, _, _) -> ev)
                case ast of
                    EX.Var (EX.Identifier x)
                        | x `elem` params -> return $ Z.Var (Z.Identifier x)
                        | otherwise -> lookup E (Solver.Evidence x) >>= \case
                            Just (Solver.Protocol (P.Implementation (P.Name id, ty, expr))) -> return $ Z.Var (Z.Identifier id)
                            Just ev -> Eff.throwError $ UnexpectedEvidence ev "Expected Protocol Implementation"

                            Nothing -> return $ Z.Var (Z.Identifier x)

                    EX.Application f args -> Z.Application <$> zonk f <*> mapM zonk args
                    EX.Lambda params body -> Z.Lambda params <$> zonk body

                    EX.List elems -> Z.List <$> mapM zonk elems
                    EX.Tuple elems -> Z.Tuple <$> mapM zonk elems
                    EX.Record pairs -> Z.Record <$> mapM2 zonk pairs

                    EX.Match subject cases -> Z.Match <$> zonk subject <*> mapM zonk cases

                    EX.Block stmts -> Z.Block <$> mapM zonk stmts

                    e -> crash $ NotYetImplemented $ "Zonking for " ++ show e

            addMissingProtocol params expr ann' = do
                Saga { protocols } <- Eff.ask
                types <- params
                        ||> fmap (\(param, prtcl, t) ->
                            ( prtcl
                            , protocols ||> List.find (\P.Protocol { id } -> id == prtcl)
                                        |$> \P.Protocol { spec } -> do
                                            zt <- zonk $ AST.Raw t
                                            -- | HACK This needs to evaluate the application of the types, but we don't have that yet
                                            return $ Z.Annotated (ZT.Applied (Z.Raw $ ZT.Polymorphic spec) zt) (Z.Raw ZK.Kind)))
                        |> mapM2 sequence
                types' <- forM types $ \(prtcl, t) ->
                        let err = Eff.throwError $ MissingProtocol prtcl
                        in maybe err return t

                let annotation = types' ||> foldl (\ty t -> Z.Annotated (ZT.Arrow t ty) (Z.Raw ZK.Kind) ) ann'

                body <- zonk' expr
                let lambda = Z.Lambda (params ||> fmap (\(id, _, _) -> id)) (Z.Annotated body ann')
                return $ Z.Annotated lambda annotation



instance Zonk (NT.Case NT.Expression) where
    type Effects (NT.Case NT.Expression) es = Zonking es
    zonk node = case node of
        AST.Raw expr           -> Z.Raw <$> zonk' expr
        AST.Annotated expr ann -> Z.Annotated <$> zonk' expr <*> zonk ann
        where
            zonk' (EX.Case pat e) = Z.Case <$> zonk pat <*> zonk e

instance Zonk (NT.Pattern NT.Expression) where
    type Effects (NT.Pattern NT.Expression) es = Zonking es

    zonk node = case node of
        AST.Raw expr           -> Z.Raw <$> zonk' expr
        AST.Annotated expr ann -> Z.Annotated <$> zonk' expr <*> zonk ann
        where
            zonk' (EX.PatTuple pats rest)   = Z.PatTuple <$> mapM zonk pats <*> pure rest
            zonk' (EX.PatList pats rest)    = Z.PatList <$> mapM zonk pats <*> pure rest
            zonk' (EX.PatRecord pairs rest) = Z.PatRecord <$> mapM2 zonk pairs <*> pure rest
            zonk' (EX.PatData tag pats)     = Z.PatData tag <$> mapM zonk pats

            zonk' (EX.PatLit lit)   = return $ Z.PatLit lit
            zonk' (EX.PatHole hole) = return $ Z.PatHole hole
            zonk' EX.Wildcard       = return  Z.Wildcard



instance Zonk NT.Statement where
    type Effects NT.Statement es = Zonking es

    zonk node = case node of
        AST.Raw stmt           -> Z.Raw <$> zonk' stmt
        AST.Annotated stmt ann -> Z.Annotated <$> zonk' stmt <*> zonk ann

        where
            zonk' (EX.Return e)      = Z.Return <$> zonk e
            zonk' (EX.Procedure e)   = Z.Procedure <$> zonk e
            zonk' (EX.Declaration d) = do
                Z.Raw d' <- zonk (AST.Raw d)
                return $ Z.Declaration d'


instance Zonk NT.Declaration where
    type Effects NT.Declaration es = Zonking es

    zonk (AST.Raw decl)  = Z.Raw <$> zonk' decl
        where
            zonk' (EX.Let id e)  = Z.Let id <$> zonk e
            zonk' (EX.Type id t) = Z.Type id <$> zonk t
            -- zonk' (EX.Kind id k) = Z.Kind id <$> zonk k






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


