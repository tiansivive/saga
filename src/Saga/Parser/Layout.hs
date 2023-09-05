
module Saga.Parser.Layout where

import qualified Saga.Lexer.Lexer as L

type LayoutStack = [(L.AlexPosn, LayoutDelimiter)]

data LayoutDelimiter
  = LytRoot
  | LytTopDecl
  | LytTopDeclHead
  | LytDeclGuard
  | LytCase
  | LytCaseBinders
  | LytCaseGuard
  | LytLambdaBinders
  | LytParens
  | LytBrace
  | LytSquare
  | LytIf
  | LytThen
  | LytProperty
  | LytForall
  | LytTick
  | LytLet
  | LytLetStmt
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo
  deriving (Show, Eq, Ord)
