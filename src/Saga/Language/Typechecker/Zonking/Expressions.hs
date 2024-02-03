{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Expressions where

import           Control.Monad                                 (forM)
import qualified Data.List                                     as List
import           Data.Maybe                                    (fromMaybe)
import           Effectful                                     (Eff)
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import           Prelude                                       hiding (id)
import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      (Phase (..))
import qualified Saga.Language.Syntax.Elaborated.AST           as AST
import           Saga.Language.Syntax.Elaborated.AST           (AST)
import qualified Saga.Language.Syntax.Elaborated.Types         as ET
import qualified Saga.Language.Syntax.Protocols                as P
import           Saga.Language.Syntax.Protocols                (Protocol (spec),
                                                                ProtocolID, id)
import qualified Saga.Language.Syntax.Zonked.AST               as Z
import qualified Saga.Language.Syntax.Zonked.Kinds             as ZK
import qualified Saga.Language.Syntax.Zonked.Types             as Z
import qualified Saga.Language.Syntax.Zonked.Values            as Z
import           Saga.Language.Typechecker.Env                 (CompilerState (..))
import           Saga.Language.Typechecker.Errors              (SagaError (MissingProtocol))
import qualified Saga.Language.Typechecker.Solving.Constraints as Solver
import           Saga.Language.Typechecker.Zonking.Monad       (Context (..),
                                                                Zonk (..),
                                                                Zonking)
import           Saga.Utils.Common                             (mapM2)
import           Saga.Utils.Operators                          ((|$>), (|>),
                                                                (||>))

import           Saga.Language.Typechecker.Zonking.Types


instance Zonk NT.Expression where
    type Effects NT.Expression es = Zonking es
    zonk (AST.Annotated expr ann) = do
        Context { solution, residuals } <- Eff.ask
        ann' <- zonk ann
        case protocol residuals of
            []     -> Z.Annotated <$> zonk' expr <*> pure ann'
            params -> addMissingProtocolFrom params ann'

        where
            zonk' :: Zonking es => Z.Node Elaborated NT.Expression -> Eff es (Z.Node Zonked NT.Expression)
            zonk' = undefined


            addMissingProtocolFrom params ann' = do
                body <- zonk' expr
                Saga { protocols } <- Eff.ask
                types <- params
                        ||> fmap (\(param, prtcl, t) -> (prtcl,
                            protocols ||> List.find (\P.Protocol { id } -> id == prtcl)
                                      |$> \P.Protocol { spec } -> do
                                            zt <- zonk $ AST.Raw t
                                            return $ Z.Annotated (Z.Applied (Z.Raw $ Z.Polymorphic spec) zt) (Z.Raw ZK.Kind)))
                        |> mapM2 sequence
                types' <- forM types $ \(prtcl, t) ->
                        let err = Eff.throwError $ MissingProtocol prtcl
                        in maybe err return t

                let annotation = types' ||> foldl (\ty t -> Z.Annotated (Z.Arrow t ty) (Z.Raw ZK.Kind) ) ann'
                let body' = Z.Annotated body ann'
                let lambda = Z.Lambda (params ||> fmap (\(id, _, _) -> id)) body'
                return $ Z.Annotated lambda annotation


protocol ::  [Solver.Constraint] -> [(String, ProtocolID, ET.Type)]
protocol cs = [(ev, pid, t) | Solver.Implementation (Solver.Evidence ev) t pid <- cs]


