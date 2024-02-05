{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Zonking where
import qualified Saga.Language.Core.Expr                       as AST
import           Saga.Language.Core.Expr                       (Expr (..))
import           Saga.Language.Typechecker.Solver.Monad        (Solution (..),
                                                                SolverEff,
                                                                Tag (..))

import qualified Data.Map                                      as Map
import qualified Effectful.State.Static.Local                  as Eff
import           Saga.Utils.Operators                          ((|>), (||>))

import           Control.Applicative                           ((<|>))
import qualified Data.List                                     as List
import           Data.Maybe                                    (isNothing)
import           Debug.Pretty.Simple                           (pTraceM)
import           Effectful                                     (Eff, (:>))
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Reader.Static                       as Eff
import           Prelude                                       hiding (lookup)
import           Saga.Language.Typechecker.Errors              (SagaError (..))
import           Saga.Language.Typechecker.Monad               (TypeCheck)
import qualified Saga.Language.Typechecker.Protocols           as P
import qualified Saga.Language.Typechecker.Solver.Constraints  as C
import           Saga.Language.Typechecker.Solver.Constraints  (Constraint,
                                                                Evidence)
import           Saga.Language.Typechecker.Solver.Substitution (Substitutable (..))
import           Saga.Language.Typechecker.Type                (Type)
import qualified Saga.Language.Typechecker.Variables           as Var
import           Saga.Language.Typechecker.Variables           (Variable)
import           Saga.Utils.TypeLevel                          (type (§))

type ZonkingEff es = (TypeCheck es, Eff.Reader Context :> es)
type Zonking a = forall es. ZonkingEff es => Eff es a

data Context = Context { solution:: Solution, residuals :: [Constraint] } deriving Show
type ScopedVar = String




-- lookup :: Tag a -> Variable a -> Zonking (Maybe a)
-- lookup T k = Eff.asks $ solution |> tvars |> Map.lookup k
-- lookup E k = do
--     Solution { evidence, witnessed } <- Eff.asks solution
--     return $ cycle (evidence, witnessed) 0 k

--     where
--         cycle (evidence, witnessed) count k
--             | count > Map.size evidence = Nothing
--             | otherwise                 = Map.lookup k evidence
--                                         <|> (Map.lookup k witnessed >>= cycle (evidence, witnessed) (count + 1))


-- zonk :: Expr -> Zonking Expr
-- zonk ast = do
--     Context { solution, residuals } <- Eff.ask
--     case evidenceParameters residuals of
--         []     -> zonk' ast
--         params -> AST.Lambda params <$> zonk' ast

--     where
--         zonk' ast = do

--             Context { solution, residuals } <- Eff.ask
--             case ast of
--                 AST.Identifier x -> do
--                     if x `elem` evidenceParameters residuals then
--                         return $ AST.Identifier x
--                     else lookup E (C.Evidence x) >>= \case
--                             Just (C.Protocol (P.Implementation (P.Name id, ty, expr))) -> return $ AST.Identifier id
--                             Just ev -> Eff.throwError $ UnexpectedEvidence ev "Expected Protocol Implementation"
--                             -- TODO: This is a legit case, but E.Identifier should represent what it's holding: a true identifier, an evidence var, some other var, etc
--                             -- Nothing -> Eff.throwError $ EvidenceNotFound x
--                             Nothing -> return $ AST.Identifier x

--                 AST.Typed e ty -> do
--                     zonked <- zonk' e
--                     return $ AST.Typed zonked $ apply (tvars solution) ty

--                 AST.FnApp fn args -> AST.FnApp <$> zonk' fn <*> mapM zonk' args
--                 AST.Lambda params body -> AST.Lambda params <$> zonk' body

--                 AST.List elems -> AST.List <$> mapM zonk' elems
--                 AST.Tuple elems -> AST.Tuple <$> mapM zonk' elems
--                 AST.Record pairs -> AST.Record <$> mapM (mapM zonk') pairs

--                 AST.Match scrutinee cases -> AST.Match <$> zonk' scrutinee <*> mapM caseZonking cases

--                 AST.Block stmts -> AST.Block <$> mapM stmtZonking stmts

--                 e -> return e


--         caseZonking (AST.Case pat e) = AST.Case <$> patZonking pat <*> zonk' e
--         caseZonking (AST.TypedCase pat ty e) = do
--             Context { solution, residuals } <- Eff.ask
--             let ty' = apply (tvars solution) ty
--             AST.TypedCase <$> patZonking pat <*> pure ty' <*> zonk' e

--         patZonking (AST.PatTuple pats rest) = AST.PatTuple <$> mapM patZonking pats <*> pure rest
--         patZonking (AST.PatList pats rest) = AST.PatList <$> mapM patZonking pats <*> pure rest
--         patZonking (AST.PatRecord pairs rest) = AST.PatRecord <$> mapM (mapM (mapM patZonking)) pairs <*> pure rest
--         patZonking (AST.PatData tag pats) = AST.PatData tag <$> mapM patZonking pats
--         patZonking (AST.TypedPat pat ty) = do
--             Context { solution, residuals } <- Eff.ask
--             let ty' = apply (tvars solution) ty
--             AST.TypedPat <$> patZonking pat <*> pure ty'

--         patZonking pat = return pat


--         stmtZonking (AST.Return e)      = AST.Return <$> zonk' e
--         stmtZonking (AST.Procedure e)   = AST.Procedure <$> zonk' e
--         stmtZonking (AST.Declaration d) = AST.Declaration <$> decZonking d

--         -- | NOTE: Annotations overrule inference, so we don't apply substitutions to them
--         decZonking (AST.Let id tyExpr kindExpr expr) = AST.Let id tyExpr kindExpr <$> zonk' expr
--         decZonking t@(AST.Type {}) = return t
--         -- | TODO: We will remove data decs
--         decZonking d@(AST.Data {}) = return d



-- evidenceParameters ::  [Constraint] -> [String]
-- evidenceParameters cs = [id | C.Impl (C.Evidence id) _ _ <- cs]
