

module Saga.Parser.ParserSpec where






import qualified Saga.Parser.Parser                      as P

import qualified Saga.Parser.Expr as E
import qualified Saga.Parser.Types as T
import qualified Saga.Parser.Literals as L


import           System.IO
import           Test.Hspec


import           Saga.Parser.Shared                 (ParsedData (..))

parse :: String -> E.Expr
parse str = case P.runSagaExpr str of
    Left msg             -> error msg
    Right (Parsed e _ _) -> e

parseDec :: String -> E.Declaration
parseDec str = case P.runSagaDec str of
    Left msg               -> error msg
    Right (Parsed dec _ _) -> dec

parseType :: String -> T.TypeExpr
parseType str = case P.runSagaType str of
    Left msg              -> error msg
    Right (Parsed ty _ _) -> ty


parseScript :: FilePath -> IO E.Expr
parseScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    hClose handle
    return $ parse contents

spec :: Spec
spec = do
  it "skipping parsing" $ 
    1 `shouldBe` 1
--   describe "Basic: " $ it "can parse identifiers"$ do
--       let E.Identifier x = parse "x"
--       x  `shouldBe` "x"

--   describe "Literal terms:" $ do
--     it "can parse integers" $ do
--         let E.Literal (L.LInt z) = parse "0"
--         z `shouldBe` 0
--         let E.Literal (L.LInt life) = parse "42"
--         life `shouldBe` 42
--         let E.Literal (L.LInt devil) = parse "666"
--         devil `shouldBe` 666

--     it "can parse strings" $ do
--         let E.Literal (L.LString hello) = parse "\"hello world\""
--         hello `shouldBe` "hello world"
--         let E.Literal (L.LString weird) = parse "\"[]{}!#$%&/()=\""
--         weird `shouldBe` "[]{}!#$%&/()="
--         -- let E.Literal (L.LString emoji) = parse "\"😂👍🤷⭐😁\""
--         -- emoji `shouldBe` "😂👍🤷⭐😁"

--     it "can parse bool" $ do
--         let E.Literal (L.LBool true) = parse "true"
--         let E.Literal (L.LBool on)   = parse "on"
--         let E.Literal (L.LBool yay)  = parse "yes"
--         true `shouldBe` True
--         on   `shouldBe` True
--         yay  `shouldBe` True
--         let E.Literal (L.LBool false) = parse "false"
--         let E.Literal (L.LBool off)   = parse "off"
--         let E.Literal (L.LBool nay)  = parse "no"
--         false `shouldBe` False
--         off   `shouldBe` False
--         nay   `shouldBe` False

--     -- it "can parse lists" $ do
--     --     let E.Literal (L.LList empty) = parse "[]"
--     --     empty `shouldBe` []
--     --     let E.Literal (L.LList [E.Literal (L.LInt singleton)]) = parse "[42]"
--     --     singleton `shouldBe` 42
--     --     let E.Literal (L.LList [ E.Literal (L.LInt one)
--     --                              , E.Literal (L.LInt two)
--     --                              , E.Literal (L.LInt three)
--     --                              ]) = parse "[1,2,3]"
--     --     [one, two, three] `shouldBe` [1,2,3]

--     it "can parse tuples" $ do
--         let E.Tuple [ E.Literal (L.LInt x)
--                                   , E.Literal (L.LInt y)
--                                   ] = parse "(1,0)"
--         (x, y) `shouldBe` (1, 0)
--         let E.Tuple [ E.Literal (L.LBool bool)
--                                   , E.Literal (L.LString str)
--                                   , E.Literal (L.LInt z)
--                                   ] = parse "(yes,\"string\",0)"
--         (bool, str, z) `shouldBe` (True, "string", 0)

--     it "can parse records" $ do


--         let E.Record empty = parse "{}"
--         empty `shouldBe` []

--         let E.Record [ (frodo, E.Literal (L.LString baggins)) ] = parse "{ frodo: \"Baggins\" }"
--         (frodo, baggins) `shouldBe` ("frodo", "Baggins")

--         let E.Record [ (ring      , E.Literal (L.LInt one))
--                                    , (lord      , E.Literal (L.LString sauron))
--                                    , (defeated  , E.Literal (L.LBool no))

--                                    ] = parse "{ ring: 1, lord: \"Sauron\", defeated: no }"
--         (one, ring) `shouldBe` (1, "ring")
--         (lord, sauron) `shouldBe` ("lord", "Sauron")
--         (defeated, no) `shouldBe` ("defeated", False)


--   describe "Functions:" $ do
--     it "can parse lambdas" $ do
--         let E.Lambda [arg] (E.Identifier (arg')) = parse "\\param -> param"
--         arg `shouldBe` "param"
--         arg' `shouldBe` "param"
--         let E.Lambda [arg1, arg2] (E.Identifier (body)) = parse "\\arg1 arg2 -> body"
--         arg1 `shouldBe` "arg1"
--         arg2 `shouldBe` "arg2"
--         body `shouldBe` "body"

--     it "can parse function application" $ do
--         let E.FnApp (E.Identifier (fn)) [] = parse "fn!"
--         fn `shouldBe` "fn"
--         let E.FnApp (E.Identifier (fn1)) [E.Identifier (arg)] = parse "fn arg!"
--         fn1 `shouldBe` "fn"
--         arg `shouldBe` "arg"
--         let E.FnApp (E.Identifier (fn2)) [E.Identifier (arg1), E.Identifier (arg2)] = parse "fn arg1 arg2!"
--         fn2 `shouldBe` "fn"
--         arg1 `shouldBe` "arg1"
--         arg2 `shouldBe` "arg2"

--   -- describe "Assignments: " $ it "can parse variable assignments" $ do
--   --     let T.Assign (life) (E.Literal (L.LInt x)) = parse "life = 42"
--   --     x `shouldBe` 42
--   --     life `shouldBe` "life"

--   describe "Top level: " $ do
--     it "can parse declarations" $ do
--       let E.Let life Nothing Nothing (E.Literal (L.LInt x)) = parseDec "let life = 42"
--       x `shouldBe` 42
--       life `shouldBe` "life"

--     -- it "can parse protocol implementations" $ do
--     --   let Let
--     --         id
--     --         (Just (T.Type (T.TImplementation protocol (T.Type ty) [])))
--     --         Nothing
--     --         T.Record r = parseDec "let functorListImpl: impl Functor: List = { }"

--     --   let functor = protocol
--     --   let T.TIdentifier (tyId) = ty

--     --   r `shouldBe` []
--     --   id `shouldBe` "functorListImpl"
--     --   functor `shouldBe` "Functor"
--     --   tyId `shouldBe` "List"


--     -- it "can parse data definitions" $ do
--     --   let Data one Nothing [(love, T.TIdentifier a)] = parseDec "data One = Love: a"
--     --   one `shouldBe` "One"
--     --   a `shouldBe` "a"
--     --   love `shouldBe` "Love"

--     -- it "can parse sum datatype definitions" $ do
--     --   let Data
--     --         sum
--     --         Nothing
--     --         [ (one, T.Type (T.TIdentifier int))
--     --         , (two, T.Type (T.TIdentifier string))
--     --         ] = parseDec "data Sum = One: Int | Two: String"

--     --   sum `shouldBe` "Sum"
--     --   one `shouldBe` "One"
--     --   two `shouldBe` "Two"
--     --   int `shouldBe` T.TInt
--     --   string `shouldBe` T.TString

--     it "can parse type level definitions" $ do
--       let E.Type t Nothing lambda = parseDec "type T = \\f => f Int!"
--       let T.TLambda [f1] (T.TFnApp fn args) = lambda
--     --   let T.Type (T.TIdentifier (f2)) = fn
--       let [T.TIdentifier int] = args

--       t `shouldBe` "T"
--       f1 `shouldBe` "f"
--     --   f1 `shouldBe` f2
--       int `shouldBe` "Int"


  describe "Type Expressions: " $ do
    it "can parse type identifiers" $ do
        let (T.TIdentifier int) = parseType "Int"
        let (T.TIdentifier some) = parseType "something"

        int `shouldBe` "Int"
        some `shouldBe` "something"
    
    it "can parse type field access" $ do
        let (T.TFnApp (T.TIdentifier op) [T.TIdentifier left, T.TIdentifier right]) = parseType "Record.Field"
       
        op `shouldBe` "."
        left `shouldBe` "Record"
        right `shouldBe` "Field"

    -- it "can parse primitive types" $ do


--     -- it "can parse parametric types" $ do
--     --     let T.Type (T.TClosure id args) = parseType "List a!"
--     --     let T.Type (T.TIdentifier (list)) = id
--     --     list `shouldBe` "List"
--     --     let  [T.Type (T.TVar (a))] = args
--     --     a `shouldBe` "a"


--     it "can parse arrow types" $ do
--         let T.TIdentifier int `T.TEArrow` T.TIdentifier string = parseType "Int -> String"
--         int `shouldBe` "Int"
--         string `shouldBe` "String"


--     it "can parse lambda types" $ do
--         let T.TLambda [param] (T.TIdentifier a `T.TEArrow` T.TIdentifier string) = parseType "\\a => a -> String"
--         param `shouldBe` "a"
--         a `shouldBe` "a"
--         string `shouldBe` "String"

--     it "can parse applied types" $ do
--         let T.TFnApp (T.TIdentifier f) [T.TIdentifier int] = parseType "f Int!"
--         f `shouldBe` "f"
--         int `shouldBe` "Int"


--     -- it "can parse qualified constrained types" $ do
--     --     let T.Type (T.TConstrained qualifiers constraints ty) = parseType "forall f a, exists b. (Functor f, a implements Show), a |-> Obj => f a! -> b"
--     --     let
--     --         [ T.TPolyVar T.Forall T.None (f)
--     --             , T.TPolyVar T.Forall T.None (a)
--     --             , T.TPolyVar T.Exists T.None (b)
--     --             ] = qualifiers
--     --     let
--     --         [ T.Implements (T.Type (T.TIdentifier (functor)))  (T.Type (T.TVar (f1)))
--     --             , T.Implements (T.Type (T.TIdentifier (show))) (T.Type (T.TVar (a1)))
--     --             , T.Extends (T.Type (T.TIdentifier (obj))) (T.Type (T.TVar (a2)))
--     --             ] = constraints

--     --     let T.Type (T.TEArrow (T.TFnApp fn body) (T.Type (T.TVar (b1)))) = ty

--     --     f `shouldBe` "f"
--     --     a `shouldBe` "a"
--     --     b `shouldBe` "b"
--     --     f1 `shouldBe` "f"
--     --     a1 `shouldBe` "a"
--     --     a2 `shouldBe` "a"
--     --     b1 `shouldBe` "b"
--     --     functor `shouldBe` "Functor"
--     --     show `shouldBe` "Show"
--     --     obj `shouldBe` "Obj"





