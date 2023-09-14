{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Saga.Parser.Types where
import           Data.Map                    (Map)
import           Saga.Language.Core.Literals (Literal)



data TypeExpr where
  TLiteral :: Literal -> TypeExpr
  TIdentifier :: String -> TypeExpr
  TETuple :: [TypeExpr] -> TypeExpr
  TERecord :: [(String, TypeExpr)] -> TypeExpr
  TEArrow :: TypeExpr -> TypeExpr -> TypeExpr
  TConditional :: TypeExpr -> TypeExpr -> TypeExpr -> TypeExpr
  TClause      :: TypeExpr -> [Binding TypeExpr] -> TypeExpr
  TEUnion        :: [TypeExpr] -> TypeExpr
  TTagged       :: String -> TypeExpr -> TypeExpr
  TLambda :: [String] -> TypeExpr -> TypeExpr
  TFnApp :: TypeExpr -> [TypeExpr] -> TypeExpr
  TImplementation :: ProtocolId -> TypeExpr -> TypeExpr


data Binding a
  = Bind String a
  | ImplBind TypeExpr String
  | SubtypeBind String a
  | RefineBind String a
  deriving (Show, Eq)

data Kind
  = KType
  | KArrow Kind Kind
  | KProtocol
  | KVar String
    deriving (Show, Eq, Ord)

type ProtocolId = String

deriving instance Show TypeExpr
deriving instance Eq TypeExpr
