

module Saga.Parser.ParserSpec where

import qualified Saga.AST.Syntax    as AST
import qualified Saga.AST.Scripts   as Scripts
import qualified Saga.AST.TypeSystem.Types    as Ty
import qualified Saga.Lexer.Lexer   as L
import qualified Saga.Parser.Parser as P
import           System.IO
import           Test.Hspec

parse :: String -> AST.Expr L.Range
parse str = case P.runSagaExpr str of
    Left msg -> error msg
    Right e  -> e

parseDec :: String -> Scripts.Declaration L.Range
parseDec str = case P.runSagaDec str of
    Left msg -> error msg
    Right e  -> e


parseScript :: FilePath -> IO (AST.Expr L.Range)
parseScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    hClose handle
    return $ parse contents

spec :: Spec
spec = do
  describe "Basic: " $ it "can parse identifiers"$ do
      let AST.Identifier (AST.Name _ x) = parse "x"
      x  `shouldBe` "x"

  describe "Literal terms:" $ do
    it "can parse integers" $ do
        let AST.Term (AST.LInt _ z) = parse "0"
        z `shouldBe` 0
        let AST.Term (AST.LInt _ life) = parse "42"
        life `shouldBe` 42
        let AST.Term (AST.LInt _ devil) = parse "666"
        devil `shouldBe` 666

    it "can parse strings" $ do
        let AST.Term (AST.LString _ hello) = parse "\"hello world\""
        hello `shouldBe` "hello world"
        let AST.Term (AST.LString _ weird) = parse "\"[]{}!#$%&/()=\""
        weird `shouldBe` "[]{}!#$%&/()="
        let AST.Term (AST.LString _ emoji) = parse "\"😂👍🤷⭐😁\""
        emoji `shouldBe` "😂👍🤷⭐😁"

    it "can parse bool" $ do
        let AST.Term (AST.LBool _ true) = parse "true"
        let AST.Term (AST.LBool _ on)   = parse "on"
        let AST.Term (AST.LBool _ yay)  = parse "yes"
        true `shouldBe` True
        on   `shouldBe` True
        yay  `shouldBe` True
        let AST.Term (AST.LBool _ false) = parse "false"
        let AST.Term (AST.LBool _ off)   = parse "off"
        let AST.Term (AST.LBool _ nay)  = parse "no"
        false `shouldBe` False
        off   `shouldBe` False
        nay   `shouldBe` False

    it "can parse lists" $ do
        let AST.Term (AST.LList _ empty) = parse "[]"
        empty `shouldBe` []
        let AST.Term (AST.LList _ [AST.Term (AST.LInt _ singleton)]) = parse "[42]"
        singleton `shouldBe` 42
        let AST.Term (AST.LList _ [ AST.Term (AST.LInt _ one)
                                 , AST.Term (AST.LInt _ two)
                                 , AST.Term (AST.LInt _ three)
                                 ]) = parse "[1,2,3]"
        [one, two, three] `shouldBe` [1,2,3]

    it "can parse tuples" $ do
        let AST.Term (AST.LTuple _ [ AST.Term (AST.LInt _ x)
                                  , AST.Term (AST.LInt _ y)
                                  ]) = parse "(1,0)"
        (x, y) `shouldBe` (1, 0)
        let AST.Term (AST.LTuple _ [ AST.Term (AST.LBool _ bool)
                                  , AST.Term (AST.LString _ str)
                                  , AST.Term (AST.LInt _ z)
                                  ]) = parse "(yes,\"string\",0)"
        (bool, str, z) `shouldBe` (True, "string", 0)

    it "can parse records" $ do


        let AST.Term (AST.LRecord _ empty) = parse "{}"
        empty `shouldBe` []
        let AST.Term (AST.LRecord _ [ (AST.Name _ frodo, AST.Term (AST.LString _ baggins))
                                   ]) = parse "{ frodo: \"Baggins\" }"
        (frodo, baggins) `shouldBe` ("frodo", "Baggins")
        let AST.Term (AST.LRecord _ [ (AST.Name _ ring      , AST.Term (AST.LInt _ one))
                                   , (AST.Name _ lord      , AST.Term (AST.LString _ sauron))
                                   , (AST.Name _ defeated  , AST.Term (AST.LBool _ no))
                                   , (AST.Name _ army      , AST.Term (AST.LList _ [ AST.Term (AST.LString _ orcs)
                                                                                  , AST.Term (AST.LString _ wargs)
                                                                                  ]))
                                   ]) = parse "{ ring: 1, lord: \"Sauron\", defeated: no, army: [\"orcs\", \"wargs\"] }"
        (one, ring) `shouldBe` (1, "ring")
        (lord, sauron) `shouldBe` ("lord", "Sauron")
        (defeated, no) `shouldBe` ("defeated", False)
        (army, orcs, wargs) `shouldBe` ("army", "orcs", "wargs")

  describe "Functions:" $ do
    it "can parse lambdas" $ do
        let AST.Lambda _ [AST.Name _ arg] (AST.Identifier (AST.Name _ arg')) = parse "\\param -> param"
        arg `shouldBe` "param"
        arg' `shouldBe` "param"
        let AST.Lambda _ [AST.Name _ arg1, AST.Name _ arg2] (AST.Identifier (AST.Name _ body)) = parse "\\arg1 arg2 -> body"
        arg1 `shouldBe` "arg1"
        arg2 `shouldBe` "arg2"
        body `shouldBe` "body"

    it "can parse function application" $ do
        let AST.FnApp _ (AST.Identifier (AST.Name _ fn)) [] = parse "fn!"
        fn `shouldBe` "fn"
        let AST.FnApp _ (AST.Identifier (AST.Name _ fn1)) [AST.Identifier (AST.Name _ arg)] = parse "fn arg!"
        fn1 `shouldBe` "fn"
        arg `shouldBe` "arg"
        let AST.FnApp _ (AST.Identifier (AST.Name _ fn2)) [AST.Identifier (AST.Name _ arg1), AST.Identifier (AST.Name _ arg2)] = parse "fn arg1 arg2!"
        fn2 `shouldBe` "fn"
        arg1 `shouldBe` "arg1"
        arg2 `shouldBe` "arg2"

  describe "Assignments: " $ it "can parse variable assignments" $ do
      let AST.Assign (AST.Name _ life) (AST.Term (AST.LInt _ x)) = parse "life = 42"
      x `shouldBe` 42
      life `shouldBe` "life"

  describe "Top level: " $ it "can parse declarations" $ do
      let Scripts.Define _ (AST.Name _ life) (AST.Term (AST.LInt _ x)) _ = parseDec "let life = 42"
      x `shouldBe` 42
      life `shouldBe` "life"



