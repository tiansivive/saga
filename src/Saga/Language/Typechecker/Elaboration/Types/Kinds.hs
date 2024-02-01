{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}
module Saga.Language.Typechecker.Elaboration.Types.Kinds where


import           Control.Monad                                 (forM)
import           Effectful                                     (Eff)
import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import           Saga.Language.Syntax.AST                      hiding
                                                               (NodeType (..))
import qualified Saga.Language.Syntax.Elaborated.AST           as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EL
import qualified Saga.Language.Syntax.Reduced.AST              as AST
import qualified Saga.Language.Syntax.Reduced.Kinds            as RD
import           Saga.Language.Syntax.Reduced.Types            (TypeExpr)

import qualified Effectful.Error.Static                        as Eff
import           Prelude                                       hiding (lookup)
import qualified Saga.Language.Typechecker.Elaboration.Effects as Effs
import           Saga.Language.Typechecker.Elaboration.Monad   (Elaboration (..))
import           Saga.Language.Typechecker.Env                 (CompilerState (..))
import           Saga.Language.Typechecker.Errors              (SagaError (..))

instance Elaboration NT.Kind where
  type Effects NT.Kind es = Effs.Elaboration es

  elaborate (AST.Raw k ) = case k of
    RD.Identifier x -> case x of
      "Type"       -> return $ EL.Raw EL.Type
      "Kind"       -> return $ EL.Raw EL.Kind
      "Constraint" -> return $ EL.Raw EL.Constraint
      _            -> Eff.throwError $ UndefinedIdentifier x

    RD.Arrow (AST.Raw -> in') (AST.Raw -> out) -> do
        in'' <- elaborate in'
        out' <- elaborate out
        return . EL.Raw $ EL.Arrow in'' out'

    Protocol [AST.Raw -> k] -> do
        k' <- elaborate k
        return . EL.Raw $ EL.Protocol k'
    Protocol [] -> Eff.throwError . Fail $ "No argument to Protocol"
    Protocol ks -> Eff.throwError . Fail $ "Too many arguments to Protocol:\n" ++ show ks

    RD.Application (AST.Raw -> cons) (fmap AST.Raw -> args) -> do
        cons' <- elaborate cons
        args' <- forM args elaborate
        return . EL.Raw $ EL.Application cons' args'


pattern Protocol ks <- RD.Application (RD.Identifier "Protocol") ks

