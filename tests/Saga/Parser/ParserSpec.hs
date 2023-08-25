

module Saga.Parser.ParserSpec where



import qualified Saga.AST.TypeSystem.HindleyMilner.Types as HM

import qualified Saga.Lexer.Lexer                        as L
import qualified Saga.Parser.ParserHM                    as P


import           System.IO
import           Test.Hspec

import           Saga.Parser.ParsingInfo                 (ParsedData (..))
import           Saga.AST.TypeSystem.HindleyMilner.Types            (Declaration (..))

parse :: String -> HM.Expr
parse str = case P.runSagaExpr str of
    Left msg             -> error msg
    Right (Parsed e _ _) -> e

parseDec :: String -> Declaration
parseDec str = case P.runSagaDec str of
    Left msg               -> error msg
    Right (Parsed dec _ _) -> dec

parseType :: String -> HM.TypeExpr
parseType str = case P.runSagaType str of
    Left msg              -> error msg
    Right (Parsed ty _ _) -> ty


parseScript :: FilePath -> IO HM.Expr
parseScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    hClose handle
    return $ parse contents

spec :: Spec
spec = do
  describe "Basic: " $ it "can parse identifiers"$ do
      let HM.Identifier x = parse "x"
      x  `shouldBe` "x"

  describe "Literal terms:" $ do
    it "can parse integers" $ do
        let HM.Term (HM.LInt z) = parse "0"
        z `shouldBe` 0
        let HM.Term (HM.LInt life) = parse "42"
        life `shouldBe` 42
        let HM.Term (HM.LInt devil) = parse "666"
        devil `shouldBe` 666

    it "can parse strings" $ do
        let HM.Term (HM.LString hello) = parse "\"hello world\""
        hello `shouldBe` "hello world"
        let HM.Term (HM.LString weird) = parse "\"[]{}!#$%&/()=\""
        weird `shouldBe` "[]{}!#$%&/()="
        -- let HM.Term (HM.LString emoji) = parse "\"😂👍🤷⭐😁\""
        -- emoji `shouldBe` "😂👍🤷⭐😁"

    it "can parse bool" $ do
        let HM.Term (HM.LBool true) = parse "true"
        let HM.Term (HM.LBool on)   = parse "on"
        let HM.Term (HM.LBool yay)  = parse "yes"
        true `shouldBe` True
        on   `shouldBe` True
        yay  `shouldBe` True
        let HM.Term (HM.LBool false) = parse "false"
        let HM.Term (HM.LBool off)   = parse "off"
        let HM.Term (HM.LBool nay)  = parse "no"
        false `shouldBe` False
        off   `shouldBe` False
        nay   `shouldBe` False

    -- it "can parse lists" $ do
    --     let HM.Term (HM.LList empty) = parse "[]"
    --     empty `shouldBe` []
    --     let HM.Term (HM.LList [HM.Term (HM.LInt singleton)]) = parse "[42]"
    --     singleton `shouldBe` 42
    --     let HM.Term (HM.LList [ HM.Term (HM.LInt one)
    --                              , HM.Term (HM.LInt two)
    --                              , HM.Term (HM.LInt three)
    --                              ]) = parse "[1,2,3]"
    --     [one, two, three] `shouldBe` [1,2,3]

    it "can parse tuples" $ do
        let HM.Tuple [ HM.Term (HM.LInt x)
                                  , HM.Term (HM.LInt y)
                                  ] = parse "(1,0)"
        (x, y) `shouldBe` (1, 0)
        let HM.Tuple [ HM.Term (HM.LBool bool)
                                  , HM.Term (HM.LString str)
                                  , HM.Term (HM.LInt z)
                                  ] = parse "(yes,\"string\",0)"
        (bool, str, z) `shouldBe` (True, "string", 0)

    it "can parse records" $ do


        let HM.Record empty = parse "{}"
        empty `shouldBe` []

        let HM.Record [ (frodo, HM.Term (HM.LString baggins)) ] = parse "{ frodo: \"Baggins\" }"
        (frodo, baggins) `shouldBe` ("frodo", "Baggins")

        let HM.Record [ (ring      , HM.Term (HM.LInt one))
                                   , (lord      , HM.Term (HM.LString sauron))
                                   , (defeated  , HM.Term (HM.LBool no))

                                   ] = parse "{ ring: 1, lord: \"Sauron\", defeated: no }"
        (one, ring) `shouldBe` (1, "ring")
        (lord, sauron) `shouldBe` ("lord", "Sauron")
        (defeated, no) `shouldBe` ("defeated", False)


  describe "Functions:" $ do
    it "can parse lambdas" $ do
        let HM.Lambda [arg] (HM.Identifier (arg')) = parse "\\param -> param"
        arg `shouldBe` "param"
        arg' `shouldBe` "param"
        let HM.Lambda [arg1, arg2] (HM.Identifier (body)) = parse "\\arg1 arg2 -> body"
        arg1 `shouldBe` "arg1"
        arg2 `shouldBe` "arg2"
        body `shouldBe` "body"

    it "can parse function application" $ do
        let HM.FnApp (HM.Identifier (fn)) [] = parse "fn!"
        fn `shouldBe` "fn"
        let HM.FnApp (HM.Identifier (fn1)) [HM.Identifier (arg)] = parse "fn arg!"
        fn1 `shouldBe` "fn"
        arg `shouldBe` "arg"
        let HM.FnApp (HM.Identifier (fn2)) [HM.Identifier (arg1), HM.Identifier (arg2)] = parse "fn arg1 arg2!"
        fn2 `shouldBe` "fn"
        arg1 `shouldBe` "arg1"
        arg2 `shouldBe` "arg2"

  -- describe "Assignments: " $ it "can parse variable assignments" $ do
  --     let HM.Assign (life) (HM.Term (HM.LInt x)) = parse "life = 42"
  --     x `shouldBe` 42
  --     life `shouldBe` "life"

  describe "Top level: " $ do
    it "can parse declarations" $ do
      let Let life Nothing Nothing (HM.Term (HM.LInt x)) = parseDec "let life = 42"
      x `shouldBe` 42
      life `shouldBe` "life"

    -- it "can parse protocol implementations" $ do
    --   let Let
    --         id
    --         (Just (HM.Type (HM.TImplementation protocol (HM.Type ty) [])))
    --         Nothing
    --         HM.Record r = parseDec "let functorListImpl: impl Functor: List = { }"

    --   let functor = protocol
    --   let HM.TIdentifier (tyId) = ty

    --   r `shouldBe` []
    --   id `shouldBe` "functorListImpl"
    --   functor `shouldBe` "Functor"
    --   tyId `shouldBe` "List"


    -- it "can parse data definitions" $ do
    --   let Data one Nothing [(love, HM.TIdentifier a)] = parseDec "data One = Love: a"
    --   one `shouldBe` "One"
    --   a `shouldBe` "a"
    --   love `shouldBe` "Love"

    -- it "can parse sum datatype definitions" $ do
    --   let Data
    --         sum
    --         Nothing
    --         [ (one, HM.Type (HM.TIdentifier int))
    --         , (two, HM.Type (HM.TIdentifier string))
    --         ] = parseDec "data Sum = One: Int | Two: String"

    --   sum `shouldBe` "Sum"
    --   one `shouldBe` "One"
    --   two `shouldBe` "Two"
    --   int `shouldBe` HM.TInt
    --   string `shouldBe` HM.TString

    it "can parse type level definitions" $ do
      let Type t Nothing lambda = parseDec "type T = \\f => f Int!"
      let HM.TLambda [f1] (HM.TFnApp fn args) = lambda
    --   let HM.Type (HM.TIdentifier (f2)) = fn
      let [HM.TIdentifier int] = args

      t `shouldBe` "T"
      f1 `shouldBe` "f"
    --   f1 `shouldBe` f2
      int `shouldBe` "Int"


  describe "Type Expressions: " $ do
    it "can parse type identifiers" $ do
        let (HM.TIdentifier int) = parseType "Int"
        let (HM.TIdentifier some) = parseType "something"

        int `shouldBe` "Int"
        some `shouldBe` "something"

    -- it "can parse primitive types" $ do


    -- it "can parse parametric types" $ do
    --     let HM.Type (HM.TClosure id args) = parseType "List a!"
    --     let HM.Type (HM.TIdentifier (list)) = id
    --     list `shouldBe` "List"
    --     let  [HM.Type (HM.TVar (a))] = args
    --     a `shouldBe` "a"


    it "can parse arrow types" $ do
        let HM.TIdentifier int `HM.TEArrow` HM.TIdentifier string = parseType "Int -> String"
        int `shouldBe` "Int"
        string `shouldBe` "String"


    it "can parse lambda types" $ do
        let HM.TLambda [param] (HM.TIdentifier a `HM.TEArrow` HM.TIdentifier string) = parseType "\\a => a -> String"
        param `shouldBe` "a"
        a `shouldBe` "a"
        string `shouldBe` "String"

    it "can parse applied types" $ do
        let HM.TFnApp (HM.TIdentifier f) [HM.TIdentifier int] = parseType "f Int!"
        f `shouldBe` "f"
        int `shouldBe` "Int"


    -- it "can parse qualified constrained types" $ do
    --     let HM.Type (HM.TConstrained qualifiers constraints ty) = parseType "forall f a, exists b. (Functor f, a implements Show), a |-> Obj => f a! -> b"
    --     let
    --         [ HM.TPolyVar HM.Forall HM.None (f)
    --             , HM.TPolyVar HM.Forall HM.None (a)
    --             , HM.TPolyVar HM.Exists HM.None (b)
    --             ] = qualifiers
    --     let
    --         [ HM.Implements (HM.Type (HM.TIdentifier (functor)))  (HM.Type (HM.TVar (f1)))
    --             , HM.Implements (HM.Type (HM.TIdentifier (show))) (HM.Type (HM.TVar (a1)))
    --             , HM.Extends (HM.Type (HM.TIdentifier (obj))) (HM.Type (HM.TVar (a2)))
    --             ] = constraints

    --     let HM.Type (HM.TEArrow (HM.TFnApp fn body) (HM.Type (HM.TVar (b1)))) = ty

    --     f `shouldBe` "f"
    --     a `shouldBe` "a"
    --     b `shouldBe` "b"
    --     f1 `shouldBe` "f"
    --     a1 `shouldBe` "a"
    --     a2 `shouldBe` "a"
    --     b1 `shouldBe` "b"
    --     functor `shouldBe` "Functor"
    --     show `shouldBe` "Show"
    --     obj `shouldBe` "Obj"





