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
zonkT node = transformM subs node
    where
        subs ::  (Zonking es) => AST Elaborated NT.Type -> Eff es (AST Elaborated NT.Type)
        subs (AST.Annotated ty ann) = do
            Context { solution } <- Eff.ask
            ann' <- zonkK ann
            return $ AST.Annotated (apply (tvars solution) ty) ann'
        subs (AST.Raw ty) = do
            Context { solution } <- Eff.ask
            return . AST.Raw $ apply (tvars solution) ty
        -- subs ast = do
        --     Context { solution } <- Eff.ask
        --     case ast of
        --         AST.Annotated ty ann -> do
        --             ann' <- zonkK ann
        --             return $ AST.Annotated (apply (tvars solution) ty) ann'
        --         AST.Raw ty -> AST.Raw <$> apply (tvars solution) ty


zonkK :: (Zonking es) => AST Elaborated NT.Kind -> Eff es (AST Elaborated NT.Kind)
--zonkK node | pTrace ("Zonking kind node: " ++ show node) False = undefined
zonkK node = transformM subs node
    where
        subs ::  (Zonking es) => AST Elaborated NT.Kind -> Eff es (AST Elaborated NT.Kind)
        subs (AST.Raw k) = do
            Context { solution } <- Eff.ask
            return . AST.Raw $ apply (kvars solution) k
        subs (AST.Annotated k ann) = error "Zonking annotated kind not yet implemented"


-- instance Zonk NT.Type where
--     type Effects NT.Type es = Zonking es
--     zonk node = do
--         Context { solution } <- Eff.ask

--         case node of
--             AST.Raw ty -> Z.Raw <$> zonk' (ty ||> apply (tvars solution))
--             AST.Annotated ty ann -> Z.Annotated <$> zonk' (apply (tvars solution) ty) <*> zonk ann

--         where
--             zonk' (ET.Singleton lit) = return $ Z.Singleton lit
--             zonk' (ET.Tuple tys)     = Z.Tuple <$> forM tys zonk
--             zonk' (ET.Union tys)     = Z.Union <$> forM tys zonk
--             zonk' (ET.Arrow in' out) = Z.Arrow <$> zonk in' <*> zonk out
--             zonk' (ET.Applied f arg) = Z.Applied <$> zonk f <*> zonk arg
--             zonk' (ET.Data con)      = return $ Z.Data con

--             zonk' (ET.Polymorphic (Forall tvars ty)) = do
--                 Z.Raw ty' <- zonk $ AST.Raw ty
--                 tvars' <- forM tvars (zonk' . ET.Var)
--                 let zvars = tvars' ||> fmap (\case Z.Var v -> v)
--                 return $ Z.Polymorphic (Forall zvars ty')

--             zonk' t@(ET.Qualified {}) = crash $ NotYetImplemented $ "Zonking qualified type: " ++ show t

--             zonk' (ET.Var (ET.Poly v k))  = zonkVar Z.Poly v k
--             zonk' (ET.Var (ET.Existential v k))  = zonkVar Z.Existential v k
--             zonk' (ET.Var (ET.Local v k))  = zonkVar Z.Local v k
--             zonk' (ET.Var (ET.Scoped v k))  = zonkVar Z.Local v k
--             zonk' (ET.Var (ET.Unification v k))  = zonkVar Z.Poly v k
--             zonk' (ET.Var (ET.Rigid v k))  = zonkVar Z.Poly v k
--             zonk' (ET.Var (ET.Skolem v k))  = zonkVar Z.Poly v k
--             zonk' (ET.Var v) = crash $ NotYetImplemented $ "Zonking type var: " ++ show v

--             zonkVar constructor v k = do
--                 Z.Raw k' <- zonk $ AST.Raw k
--                 return $ Z.Var (constructor v k')


-- instance Zonk NT.Kind where
--     type Effects NT.Kind es = Zonking es
--     zonk node = do
--         Context { solution } <- Eff.ask

--         case node of
--             AST.Raw k        -> Z.Raw <$> zonk' (k ||> apply (kvars solution))
--             AST.Annotated {} -> crash $ Unexpected "Annotated Kind:" (show node)


--         where
--             zonk' EK.Type                       = return ZK.Type
--             zonk' EK.Kind                       = return ZK.Kind
--             zonk' EK.Constraint                 = return ZK.Constraint
--             zonk' (EK.Protocol (AST.Raw k))     = ZK.Protocol <$> zonk' k

--             zonk' (EK.Arrow (AST.Raw in') (AST.Raw out)) = ZK.Arrow <$> zonk' in' <*> zonk' out
--             zonk' (EK.Application (AST.Raw in') args) = ZK.Application <$> zonk' in' <*> forM args (\(AST.Raw k) -> zonk' k)


--             zonk' (EK.Var (EK.Poly v))         = return $ ZK.Var (ZK.Poly v)
--             zonk' (EK.Var (EK.Rigid v))        = return $ ZK.Var (ZK.Poly v)
--             zonk' (EK.Var (EK.Unification v))  = return $ ZK.Var (ZK.Poly v)
