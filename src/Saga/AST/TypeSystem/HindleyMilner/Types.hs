{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.AST.TypeSystem.HindleyMilner.Types where
import           GHC.Base ()



data TypeExpr where
  Type            :: Type -> TypeExpr
  TIdentifier     :: String -> TypeExpr
  TParens         :: TypeExpr -> TypeExpr
  TConditional    :: TypeExpr -> TypeExpr -> TypeExpr -> TypeExpr
  TClause         :: [TypeExpr] -> TypeExpr -> TypeExpr
  TBlock          :: [TypeExpr] -> TypeExpr
  TReturn         :: TypeExpr -> TypeExpr
  TLambda         :: [String] -> TypeExpr -> TypeExpr
  TFnApp          :: TypeExpr -> [TypeExpr] -> TypeExpr

deriving instance Show TypeExpr
deriving instance Eq TypeExpr
-- instance Eq TypeExpr where
--     Type _ t == Type _ t'       = t == t'
--     TIdentifier _ t == TIdentifier _ t' = t == t'
--     TParens _ t == TParens _ t' = False

--     TConditional _ cond true false == TConditional _ cond' true' false' =
--         cond == cond' && true == true' && false == false'
--     TClause _ tyExprs ret == TClause _ tyExprs' ret' = tyExprs == tyExprs' && ret == ret'
--     TBlock _ tyExprs == TBlock _ tyExprs' = tyExprs == tyExprs'
--     TReturn _ tyExpr == TReturn _ e' = tyExpr == e'
--     TLambda _ params body == TLambda _ params' body' = params == params' && body == body'
--     TFnApp _ fn args == TFnApp _ fn' args' = fn == fn' && args == args'

--     _ == _ = False


data Type where
  TLiteral          :: Term -> Type
  TPrimitive        :: BuiltInType -> Type
  TTuple            :: [TypeExpr] -> Type
  TRecord           :: [(String, TypeExpr)] -> Type
  TArrow            :: TypeExpr -> TypeExpr -> Type

  TParametric       :: [String] -> TypeExpr -> Type
  TVar              :: String -> Type

  TConstrained      :: [PolymorphicVar] -> [Constraint] -> TypeExpr -> Type
  TProtocol         :: Type -> Type
  TImplementation   :: TypeExpr -> [RequiredImplId] -> Type

  TUnit             :: Type



deriving instance Show Type
instance Eq Type where
    TLiteral t == TLiteral t' = t == t'
    TPrimitive t == TPrimitive t' = t == t'
    TTuple t == TTuple t' = t == t'
    TRecord t == TRecord t' = t == t'
    TVar t == TVar t' = t == t'

    TArrow arg out == TArrow arg' out' = arg == arg' && out == out'
    TParametric args out == TParametric args' out' = args == args' && out == out'

    TProtocol p == TProtocol p' = p == p'
    TImplementation ty reqs == TImplementation  ty' reqs' = ty == ty' && reqs == reqs'
    TConstrained vars constraints ty == TConstrained vars' constraints' ty' =
        vars == vars' && constraints == constraints' && ty == ty'

    _ == _ = False


type RequiredImplId = String

data BuiltInType
    = TBool
    | TInt
    | TString
    deriving (Show, Eq)


data PolymorphicVar where
    TPolyVar :: Quantifier -> Multiplicity -> String -> PolymorphicVar

deriving instance Show PolymorphicVar
instance Eq PolymorphicVar where
    (TPolyVar _ _ id) ==  (TPolyVar _ _ id') = id == id'



data Expr where
  Term           :: Term -> Expr
  Identifier     :: String -> Expr
  Assign         :: String -> Expr -> Expr
  IfElse         :: Expr -> Expr -> Expr -> Expr
  Lambda         :: [String] -> Expr -> Expr
  FnApp          :: Expr -> [Expr] -> Expr
  Clause         :: [Expr] -> Expr -> Expr
  Block          :: [Expr] -> Expr
  Return         :: Expr -> Expr
  Parens         :: Expr -> Expr
  FieldAccess    :: Expr -> [String] -> Expr

deriving instance Show Expr
deriving instance Eq Expr
-- instance Eq Expr where
--     Term _ t == Term _ t' = t == t'
--     Identifier _ id == Identifier _ id' = id == id'
--     Assign _ id e == Assign _ id' e' = id == id' && e == e'
--     IfElse _ cond true false == IfElse _ cond' true' false' =
--         cond == cond' && true == true' && false == false'
--     Lambda _ params body == Lambda _ params' body' = params == params' && body == body'
--     FnApp _ fn args == FnApp _ fn' args' = fn == fn' && args == args'
--     Clause _ es ret == Clause _ es' ret' = es == es' && ret == ret'
--     Block _ es == Block _ es' = es == es'
--     Return _ e == Return _ e' = e == e'
--     Parens _ e == Parens _ e' = e == e'
--     FieldAccess _ e path == FieldAccess _ e' path' = e == e'  && path == path'

--     _ == _               = False


-- instance ParsingInfo Expr where
--   (<->) e e' = e
--   details _ = ParseError 0 0

data Term where
  LInt    :: Int -> Term
  LBool   :: Bool -> Term
  LString :: String -> Term
  LList   :: [Expr] -> Term
  LTuple  :: [Expr] -> Term
  LRecord :: [(String, Expr)] -> Term

deriving instance Show Term
deriving instance Eq Term


data Constraint
  = Implements TypeExpr TypeExpr
  | Extends TypeExpr TypeExpr
    deriving ( Show, Eq)

data Quantifier = Forall | Exists
  deriving (Eq, Show)
data Multiplicity =
  Erased | Linear | Affine | Relevant | Custom Int
  deriving (Eq, Show)


data Mode = Strict | Lazy








