module Saga.Language.Typechecker.Zonking.Run where
import           Control.Monad                                   (forM)
import qualified Data.Set                                        as Set
import           Effectful                                       (Eff)
import qualified Effectful                                       as Eff
import qualified Effectful.Reader.Static                         as Eff
import qualified Saga.Language.Core.Elaborated                   as Elaborated
import qualified Saga.Language.Core.Zonked                       as Zonked

import           Debug.Pretty.Simple                             (pTrace,
                                                                  pTraceM)
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Shared                as Shared
import           Saga.Language.Typechecker.Solver.Substitution   (Substitutable (..))
import           Saga.Language.Typechecker.Type                  (Polymorphic,
                                                                  Type)
import           Saga.Language.Typechecker.Zonking.Normalisation (Normalisation (..))
import           Saga.Language.Typechecker.Zonking.Qualification
import           Saga.Language.Typechecker.Zonking.Zonking       (Context (..),
                                                                  Zonking, zonk)


run :: TypeCheck es =>  Expr -> Context -> Eff es (Expr, Polymorphic Type)
run ast context@(Context { residuals, solution }) = do
    zonked <- Eff.runReader context $ zonk ast
    pTraceM $ "Zonked\n" ++ show zonked
    ast' <- Eff.runReader (mapping zonked) $ normalise zonked
    residuals' <- Eff.runReader (mapping zonked) $ forM residuals normalise

    qt <- Eff.inject $ qualify zonked residuals


    return (ast', qt)

    where
        mapping zonked = zip (ftvs zonked) Shared.letters
        ftvs zonked = Set.toList $ ftv @Expr @Type zonked <> ftv residuals
