

module Saga.Parser.ParserSpec where

import Test.Hspec
import qualified Saga.Parser.Parser as P
import qualified Saga.Lexer.Lexer as L
import qualified Saga.AST.Syntax as AST


parse :: String -> AST.Expr L.Range
parse str = case P.runSagaExpr str of
    Left msg -> error msg
    Right e -> e

spec :: Spec
spec = do
  describe "Literals:" $ do
    it "can parse integers" $ do
        let AST.Lit (AST.LInt _ val) = parse "1" 
        val `shouldBe` 1
