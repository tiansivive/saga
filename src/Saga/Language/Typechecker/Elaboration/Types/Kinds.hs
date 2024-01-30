{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Saga.Language.Typechecker.Elaboration.Types.Kinds where


import           Control.Monad                                 (forM)
import qualified Saga.Language.Syntax.AST                      as NT (NodeType (..))
import qualified Saga.Language.Syntax.Elaborated.AST           as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds         as EK
import qualified Saga.Language.Syntax.Evaluated.AST            as AST
import qualified Saga.Language.Syntax.Evaluated.Kinds          as EV
import qualified Saga.Language.Typechecker.Elaboration.Effects as Effs
import           Saga.Language.Typechecker.Elaboration.Monad   (Elaboration (..))
import           Saga.Language.Typechecker.Errors              (Exception (NotYetImplemented),
                                                                crash)

instance Elaboration NT.Kind where
  type Effects NT.Kind es = Effs.Elaboration es

  elaborate (AST.Raw k) = case k of
    EV.Type                  -> return $ EL.Raw EK.Type
    EV.Kind                  -> return $ EL.Raw EK.Kind
    EV.Constraint            -> return $ EL.Raw EK.Constraint
    EV.Protocol (AST.Raw -> k)            -> do
        k' <- elaborate k
        return $ EL.Raw (EK.Protocol k')
    EV.Arrow (AST.Raw -> in') (AST.Raw -> out)         -> do
        in'' <- elaborate in'
        out' <- elaborate out
        return . EL.Raw $ EK.Arrow in'' out'
    EV.Application (AST.Raw -> cons) (fmap AST.Raw -> args) -> do
        cons' <- elaborate cons
        args' <- forM args elaborate
        return . EL.Raw $ EK.Application cons' args'
    EV.Polymorphic poly      -> crash $ NotYetImplemented "Elaboration of Polymorphic kinds"


