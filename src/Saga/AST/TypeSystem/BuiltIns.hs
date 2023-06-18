module Saga.AST.TypeSystem.BuiltIns where
import           Saga.AST.TypeSystem.Types



int' :: TypeExpr a
int' = Type (TPrimitive undefined TInt)

operatorFnTypes :: [(String, Type a)]
operatorFnTypes =
    [ ("+", TArrow undefined int' (Type (TArrow undefined int' int')  ))

    ]
