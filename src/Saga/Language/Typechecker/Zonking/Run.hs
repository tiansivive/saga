module Saga.Language.Typechecker.Zonking.Run where
import qualified Data.Set                                        as Set
import qualified Effectful                                       as Eff
import qualified Effectful.Reader.Static                         as Eff
import           Saga.Language.Core.Expr                         (Expr)
import           Saga.Language.Typechecker.Monad                 (TypeCheck)
import qualified Saga.Language.Typechecker.Shared                as Shared
import           Saga.Language.Typechecker.Solver.Substitution   (Substitutable (..))
import           Saga.Language.Typechecker.Type                  (Polymorphic,
                                                                  Type)
import           Saga.Language.Typechecker.Zonking.Normalisation (Normalisation (..))
import           Saga.Language.Typechecker.Zonking.Qualification
import           Saga.Language.Typechecker.Zonking.Zonking       (Context (..),
                                                                  Zonking, zonk)

run :: Expr -> Context -> TypeCheck es (Expr, Polymorphic Type)
run ast context@(Context { residuals, solution }) = do
    zonked <- Eff.runReader context . Eff.inject $ zonk ast

    ast' <- Eff.runReader (mapping zonked) $ Eff.inject $ normalise zonked
    residuals' <- Eff.runReader (mapping zonked) $ Eff.inject $ mapM normalise residuals

    ty <- Eff.inject $ qualify ast' residuals'

    return (ast', ty)

    where
        mapping zonked = zip (ftvs zonked) Shared.letters
        ftvs zonked = Set.toList $ ftv @Expr @Type zonked <> ftv residuals
