{-# LANGUAGE FlexibleInstances #-}


module Saga.Language.Generation.JS where
import           Control.Monad.State
import           Data.List                           (intercalate)
import           Data.Maybe                          (fromMaybe)
import           Saga.Language.Core.Expr             hiding (Binding)
import           Saga.Language.Core.Literals         (Literal (..))
import           Saga.Language.TypeSystem.Refinement
import           Saga.Language.TypeSystem.Types      (Binding,
                                                      CompositeExpr (..),
                                                      Constraint (..),
                                                      PrimitiveType (..),
                                                      Qualified ((:=>)),
                                                      Type (..), TypeExpr (..))


class Generator a where
    generate :: a -> String




instance Generator Expr where
    generate (Literal lit)   = generate lit
    generate (Identifier id) = generate id
    generate (List exprs) = "[" ++ code exprs ++ "]"
        where code = intercalate "," . fmap generate
    generate (Tuple exprs) = "[" ++ code exprs ++ "]"
        where code = intercalate "," . fmap generate
    generate (Record pairs) = "({" ++ code pairs ++ "})"
        where
            toKeyVal (key, val) = key ++ ":" ++ val
            code = intercalate "," . fmap (toKeyVal . fmap generate)
    generate (Lambda params body) = "(" ++ intercalate "," params  ++ ") => " ++ generate body
    generate (FnApp fn args) = generate fn ++ "(" ++ intercalate "," (fmap generate args)  ++ ")"
    generate (Block stmts) = "(() => {" ++ intercalate ";\n" (fmap generate stmts) ++  "})()"
    generate (Typed e _) = generate e

instance Generator Literal where
    generate (LInt int)    = show int
    generate (LString str) = "\"" ++ str ++ "\""
    generate (LBool True)  = "true"
    generate (LBool False) = "false"

instance Generator Statement where
    generate (Return expr)     = "return " ++ generate expr
    generate (Procedure expr)  = generate expr
    generate (Declaration dec) = generate dec

instance Generator Declaration where
    generate (Type {})              = ""
    generate (Let id tyExpr _ expr) = "let " ++ id ++ " = " ++ generate expr
    -- generate (Data id k (TClause (TLambda _ ty) bindings)) = main ++ standalone constructors ++ "\n"
    --     where
    --         main = "let " ++ id ++ " = " ++ generate constructors ++ "\n"


    --         standalone (Record cs) = intercalate "\n" $ fmap code cs
    --             where
    --                 code (name, _) = "let " ++ name ++ " = " ++ id ++ "." ++ name

    --         constructors = case ty of
    --             TComposite (TEUnion tys) -> Record (fmap build tys)
    --             t@(TTagged _ _)          -> Record [build t]
    --             _                        -> error "In generate Declaration: Unexpected type expression in data declaration"

    --         build (TTagged tag ty) = (tag, Lambda args record)
    --             where
    --                 args = fmap (\n -> "_" ++ show n) [0 .. count 0 ty]
    --                 record = Record [("tag", Literal (LString tag)), ("values", Tuple $ fmap Identifier args)]

    --         count n (TComposite (TEArrow t1 t2)) = n + count n t1 + count n t2
    --         count n _                            = n
    --         --"const " ++ id ++ " = {" ++ intercalate "," (fmap generate dataExprs)  ++ "}"


instance Generator DataExpr where
    -- generate (tag, TAtom ty) = tag ++ ": (" ++ values ++ ") => ({_tag: \"" ++ tag ++ "\"," ++ values ++ "})"
    --     where
    --         params :: Type -> State Int [String]
    --         params (TArrow one two) = do
    --             one' <- params one
    --             two' <- params two
    --             return $ one' ++ two'
    --         params _ = do
    --             count <- get
    --             return ["val" ++ show count]

    --         values = intercalate "," $ evalState (params ty) 0
    generate (_, tyExpr) = error "No code generation implemented yet for this type of TypeExpr:\n" ++ show tyExpr


instance Generator Script where
    generate (Script decs) = intercalate "\n\n" $ fmap generate decs

instance Generator String where
    generate "+"  = "Core.add"
    generate "-"  = "Core.subtract"
    generate "/"  = "Core.divide"
    generate "*"  = "Core.multiply"
    generate "||" = "Core.or"
    generate "&&" = "Core.and"
    generate "<"  = "Core.lt"
    generate ">"  = "Core.gt"
    generate "<=" = "Core.gte"
    generate ">=" = "Core.gte"
    generate "==" = "Core.eq"
    generate "!=" = "Core.!eq"
    generate "|>" = "Core.pipeRight"
    generate "<|" = "Core.pipeLeft"
    generate "++" = "Core.concat"
    generate "."  = "Core.fieldAccess"
    generate id   = id



