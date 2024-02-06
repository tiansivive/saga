{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Types where
import           Saga.Language.Typechecker.Zonking.Monad (Context (..), Effects,
                                                          Zonk (..), Zonking,
                                                          solution)


import qualified Effectful.Reader.Static                 as Eff
import qualified Saga.Language.Syntax.AST                as NT (NodeType (..))
import qualified Saga.Language.Syntax.Elaborated.AST     as AST
import qualified Saga.Language.Syntax.Elaborated.Kinds   as EK
import qualified Saga.Language.Syntax.Elaborated.Types   as ET


import           Control.Monad                           (forM)
import           Data.Data                               (Data)
import           Data.Generics.Uniplate.Data             (transformBiM,
                                                          transformM)
import           Effectful                               (Eff)
import           Saga.Language.Syntax.AST                (Phase (Elaborated))
import           Saga.Language.Syntax.Elaborated.AST     (AST)
import           Saga.Language.Syntax.Polymorphism       (Polymorphic (..))
import           Saga.Language.Typechecker.Errors        (Exception (NotYetImplemented, Unexpected),
                                                          crash)
import           Saga.Language.Typechecker.Solving.Monad (Solution (..))
import           Saga.Language.Typechecker.Substitution  (Substitutable (..))
import           Saga.Utils.Common                       (forM2)
import           Saga.Utils.Operators                    ((||>))

import           Debug.Pretty.Simple                     (pTrace)
import           Saga.Language.Typechecker.Traversals


zonkT :: (Zonking es) => AST Elaborated NT.Type -> Eff es (AST Elaborated NT.Type)
--zonkT node | pTrace ("Zonking type node: " ++ show node) False = undefined
zonkT = transformM subs
    where
        subs ::  (Zonking es) => AST Elaborated NT.Type -> Eff es (AST Elaborated NT.Type)
        subs (AST.Annotated ty ann) = do
            Context { solution } <- Eff.ask
            ann' <- zonkK ann
            return $ AST.Annotated (apply (tvars solution) ty) ann'
        subs (AST.Raw ty) = do
            Context { solution } <- Eff.ask
            return . AST.Raw $ apply (tvars solution) ty

zonkK :: (Zonking es) => AST Elaborated NT.Kind -> Eff es (AST Elaborated NT.Kind)
--zonkK node | pTrace ("Zonking kind node: " ++ show node) False = undefined
zonkK = transformM subs
    where
        subs ::  (Zonking es) => AST Elaborated NT.Kind -> Eff es (AST Elaborated NT.Kind)
        subs (AST.Raw k) = do
            Context { solution } <- Eff.ask
            return . AST.Raw $ apply (kvars solution) k

