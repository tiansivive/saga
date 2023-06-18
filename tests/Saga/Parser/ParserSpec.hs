

module Saga.Parser.ParserSpec where


import qualified Saga.AST.Scripts          as Scripts
import qualified Saga.AST.Syntax           as AST
import qualified Saga.AST.TypeSystem.Types as Ty
import qualified Saga.Lexer.Lexer          as L
import qualified Saga.Parser.Parser        as P
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

parseType :: String -> Ty.TypeExpr L.Range
parseType str = case P.runSagaType str of
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

  describe "Top level: " $ do
    it "can parse declarations" $ do
      let Scripts.Let (AST.Name _ life) Nothing Nothing (AST.Term (AST.LInt _ x)) = parseDec "let life = 42"
      x `shouldBe` 42
      life `shouldBe` "life"

    it "can parse protocol implementations" $ do
      let Scripts.Let
            (AST.Name _ id)
            (Just (Ty.Type (Ty.TImplementation protocol (Ty.Type ty) [])))
            Nothing
            (AST.Term (AST.LRecord _ r)) = parseDec "let functorListImpl: impl Functor: List = { }"

      let AST.Name _ functor = protocol
      let Ty.TIdentifier (AST.Name _ tyId) = ty

      r `shouldBe` []
      id `shouldBe` "functorListImpl"
      functor `shouldBe` "Functor"
      tyId `shouldBe` "List"


    it "can parse data definitions" $ do
      let Scripts.Data (AST.Name _ one) Nothing [(AST.Name _ love, Ty.Type (Ty.TVar (AST.Name _ a)))] = parseDec "data One = Love: a"
      one `shouldBe` "One"
      a `shouldBe` "a"
      love `shouldBe` "Love"

    it "can parse sum datatype definitions" $ do
      let Scripts.Data
            (AST.Name _ sum)
            Nothing
            [ (AST.Name _ one, Ty.Type (Ty.TPrimitive _ int))
            , (AST.Name _ two, Ty.Type (Ty.TPrimitive _ string))
            ] = parseDec "data Sum = One: Int | Two: String"

      sum `shouldBe` "Sum"
      one `shouldBe` "One"
      two `shouldBe` "Two"
      int `shouldBe` Ty.TInt
      string `shouldBe` Ty.TString

    it "can parse type level definitions" $ do
      let Scripts.Type (AST.Name _ t) Nothing lambda = parseDec "type T = \\f -> f Int!"
      let Ty.TLambda _ [AST.Name _ f1] (Ty.TFnApp _ fn args) = lambda
    --   let Ty.Type (Ty.TIdentifier (AST.Name _ f2)) = fn
      let [Ty.Type (Ty.TPrimitive _ int)] = args

      t `shouldBe` "T"
      f1 `shouldBe` "f"
    --   f1 `shouldBe` f2
      int `shouldBe` Ty.TInt


  describe "Types: " $ do
    it "can parse primitive types" $ do
        let Ty.Type (Ty.TPrimitive _ int) = parseType "Int"
        let Ty.Type (Ty.TPrimitive _ bool) = parseType "Bool"
        let Ty.Type (Ty.TPrimitive _ string) = parseType "String"
        int `shouldBe` Ty.TInt
        bool `shouldBe` Ty.TBool
        string `shouldBe` Ty.TString

    -- it "can parse parametric types" $ do
    --     let Ty.Type (Ty.TParametric id args) = parseType "List a!"
    --     let Ty.Type (Ty.TIdentifier (AST.Name _ list)) = id
    --     list `shouldBe` "List"
    --     let  [Ty.Type (Ty.TVar (AST.Name _ a))] = args
    --     a `shouldBe` "a"


    it "can parse arrow types" $ do
        let Ty.Type (Ty.TArrow _ (Ty.Type (Ty.TPrimitive _ int)) (Ty.Type (Ty.TPrimitive _ string))) = parseType "Int -> String"
        int `shouldBe` Ty.TInt
        string `shouldBe` Ty.TString

    it "can parse qualified constrained types" $ do
        let Ty.Type (Ty.TConstrained qualifiers constraints ty) = parseType "forall f a, exists b. (Functor f, a implements Show), a |-> Obj => f a! -> b"
        let
            [ Ty.TPolyVar Ty.Forall Ty.None (AST.Name _ f)
                , Ty.TPolyVar Ty.Forall Ty.None (AST.Name _ a)
                , Ty.TPolyVar Ty.Exists Ty.None (AST.Name _ b)
                ] = qualifiers
        let
            [ Ty.Implements (Ty.Type (Ty.TIdentifier (AST.Name _ functor)))  (Ty.Type (Ty.TVar (AST.Name _ f1)))
                , Ty.Implements (Ty.Type (Ty.TIdentifier (AST.Name _ show))) (Ty.Type (Ty.TVar (AST.Name _ a1)))
                , Ty.Extends (Ty.Type (Ty.TIdentifier (AST.Name _ obj))) (Ty.Type (Ty.TVar (AST.Name _ a2)))
                ] = constraints

        let Ty.Type (Ty.TArrow _ (Ty.TFnApp _ fn body) (Ty.Type (Ty.TVar (AST.Name _ b1)))) = ty

        f `shouldBe` "f"
        a `shouldBe` "a"
        b `shouldBe` "b"
        f1 `shouldBe` "f"
        a1 `shouldBe` "a"
        a2 `shouldBe` "a"
        b1 `shouldBe` "b"
        functor `shouldBe` "Functor"
        show `shouldBe` "Show"
        obj `shouldBe` "Obj"





