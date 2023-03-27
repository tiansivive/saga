

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
        let AST.Lit (AST.LInt _ z) = parse "0" 
        z `shouldBe` 0
        let AST.Lit (AST.LInt _ life) = parse "42" 
        life `shouldBe` 42
        let AST.Lit (AST.LInt _ devil) = parse "666" 
        devil `shouldBe` 666

    it "can parse strings" $ do
        let AST.Lit (AST.LString _ hello) = parse "\"hello world\"" 
        hello `shouldBe` "hello world"
        let AST.Lit (AST.LString _ weird) = parse "\"[]{}!#$%&/()=\"" 
        weird `shouldBe` "[]{}!#$%&/()="
        let AST.Lit (AST.LString _ emoji) = parse "\"😂👍🤷⭐😁\"" 
        emoji `shouldBe` "😂👍🤷⭐😁"

    it "can parse bool" $ do
        let AST.Lit (AST.LBool _ true) = parse "true" 
        let AST.Lit (AST.LBool _ on)   = parse "on" 
        let AST.Lit (AST.LBool _ yay)  = parse "yes" 
        true `shouldBe` True
        on   `shouldBe` True
        yay  `shouldBe` True
        let AST.Lit (AST.LBool _ false) = parse "false" 
        let AST.Lit (AST.LBool _ off)   = parse "off" 
        let AST.Lit (AST.LBool _ nay)  = parse "no" 
        false `shouldBe` False
        off   `shouldBe` False
        nay   `shouldBe` False

    it "can parse lists" $ do
        let AST.Lit (AST.LList _ empty) = parse "[]"
        empty `shouldBe` []
        let AST.Lit (AST.LList _ [AST.Lit (AST.LInt _ singleton)]) = parse "[42]"
        singleton `shouldBe` 42
        let AST.Lit (AST.LList _ [ AST.Lit (AST.LInt _ one)
                                 , AST.Lit (AST.LInt _ two)
                                 , AST.Lit (AST.LInt _ three)
                                 ]) = parse "[1,2,3]"
        [one, two, three] `shouldBe` [1,2,3]

    it "can parse tuples" $ do
        let AST.Lit (AST.LTuple _ [ AST.Lit (AST.LInt _ x)
                                  , AST.Lit (AST.LInt _ y)
                                  ]) = parse "(1,0)"
        (x, y) `shouldBe` (1, 0)
        let AST.Lit (AST.LTuple _ [ AST.Lit (AST.LBool _ bool)
                                  , AST.Lit (AST.LString _ str)
                                  , AST.Lit (AST.LInt _ z)
                                  ]) = parse "(yes,\"string\",0)"
        (bool, str, z) `shouldBe` (True, "string", 0)

    it "can parse records" $ do
        let AST.Lit (AST.LRecord _ empty) = parse "{}" 
        empty `shouldBe` []
        let AST.Lit (AST.LRecord _ [ (AST.Name _ frodo, AST.Lit (AST.LString _ baggins))
                                   ]) = parse "{ frodo: \"Baggins\" }" 
        (frodo, baggins) `shouldBe` ("frodo", "Baggins")
        let AST.Lit (AST.LRecord _ [ (AST.Name _ ring      , AST.Lit (AST.LInt _ one))
                                   , (AST.Name _ lord      , AST.Lit (AST.LString _ sauron))
                                   , (AST.Name _ defeated  , AST.Lit (AST.LBool _ no))
                                   , (AST.Name _ army      , AST.Lit (AST.LList _ [ AST.Lit (AST.LString _ orcs)
                                                                                  , AST.Lit (AST.LString _ wargs)
                                                                                  ]))
                                   ]) = parse "{ ring: 1, lord: \"Sauron\", defeated: no, army: [\"orcs\", \"wargs\"] }" 
        (one, ring) `shouldBe` (1, "ring")
        (lord, sauron) `shouldBe` ("lord", "Sauron")
        (defeated, no) `shouldBe` ("defeated", False)
        (army, orcs, wargs) `shouldBe` ("army", "orcs", "wargs")

