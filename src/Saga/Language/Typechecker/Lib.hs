{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeFamilies #-}

module Saga.Language.Typechecker.Lib where

import qualified Saga.Language.Core.Expr                     as E
import qualified Saga.Language.Typechecker.Kind              as K
import qualified Saga.Language.Typechecker.Protocols         as P
import           Saga.Language.Typechecker.Protocols         (Protocol (..))
import           Saga.Language.Typechecker.Qualification     (Given (..),
                                                              Qualified ((:=>)))
import qualified Saga.Language.Typechecker.Type              as T
import           Saga.Language.Typechecker.Type              (Polymorphic,
                                                              Scheme (..), Type)
import qualified Saga.Language.Typechecker.TypeExpr          as TE
import qualified Saga.Language.Typechecker.Variables         as Var

import qualified Data.Map                                    as Map
import           Saga.Language.Typechecker.Environment
import qualified Saga.Language.Typechecker.Qualification     as Q
import qualified Saga.Language.Typechecker.Refinement.Liquid as L



listConstructor, fnConstructor :: Type
listConstructor = T.Data "List" (K.Arrow K.Type K.Type)
fnConstructor = T.Data "->" (K.Arrow K.Type (K.Arrow K.Type K.Type))

int, bool, string :: Type
int = T.Data "Int" K.Type
bool = T.Data "Bool" K.Type
string = T.Data "String" K.Type


eqProtocol :: Protocol
eqProtocol =
  Protocol
    "Eq"
    (TE.KindedType
      (TE.Lambda [var] (TE.Record
        [("==", tvar `TE.Arrow` (tvar `TE.Arrow` TE.Identifier "Bool"))
        ])
      )
      K.Type
    )
    [ P.Implementation (eqID, Forall [] (Map.empty :| [] :=> int),    E.Record [("==", E.Identifier "$int_$eq_$equals")])
    , P.Implementation (eqID, Forall [] (Map.empty :| [] :=> string), E.Record [("==", E.Identifier "$string_$eq_$equals")])
    , P.Implementation (eqID, Forall [] (Map.empty :| [] :=> bool),   E.Record [("==", E.Identifier "$bool_$eq_$equals")])
    ]
    where
      var = "t"
      tvar = TE.Identifier var

numProtocol :: Protocol
numProtocol =
  Protocol
    "Num"
    (TE.KindedType
      (TE.Lambda [var] (TE.Record
        [ ("+", tvar `TE.Arrow` (tvar `TE.Arrow` tvar))
        , ("-", tvar `TE.Arrow` (tvar `TE.Arrow` tvar))
        , ("*", tvar `TE.Arrow` (tvar `TE.Arrow` tvar))
        , ("/", tvar `TE.Arrow` (tvar `TE.Arrow` tvar))
        ])
      )
      K.Type
    )
    [ P.Implementation (numID, Forall [] (Map.empty :| [] :=> int), E.Record [ ("+", E.Identifier "$int_$num_$add")
                                                     , ("-", E.Identifier "$int_$num_$sub")
                                                     , ("*", E.Identifier "$int_$num_$mul")
                                                     , ("/", E.Identifier "$int_$num_$div")
                                                     ])
    ]
    where
      var = "t"
      tvar = TE.Identifier var



isStringProtocol :: Protocol
isStringProtocol =
  Protocol
    "IsString"
    (TE.KindedType
      (TE.Lambda [var] (TE.Record
        [("isString", tvar `TE.Arrow` TE.Identifier "Bool")
        ])
      )
      K.Type
    )
    [ P.Implementation ( isStringID
                       , Forall [] (Map.empty :| [] :=> string)
                       , E.Record [("isString", E.Identifier "$str_$is_string_$is_String")]
                       )
    ]
    where
        var = "t"
        tvar = TE.Identifier var

functorProtocol :: Protocol
functorProtocol =
  Protocol
    "Functor"
    (TE.KindedType
      (TE.Lambda [functor] (TE.Record
        [("map", TE.Lambda [a, b] $ fn `TE.Arrow` (fa `TE.Arrow` fb))
        ])
      )
      (K.Arrow K.Type K.Type)
    )
    [ P.Implementation ( functorID
                       , Forall [] (Map.empty :| [] :=> listConstructor)
                       , E.Record [("map", E.Identifier "$list_$functor_$map")]
                       )
    ]
      where
        functor = "f"
        a = "a"
        b = "b"
        fn = var a `TE.Arrow` var b
        fa = TE.Application (var functor) [var a]
        fb = TE.Application (var functor) [var b]
        var = TE.Identifier

semigroupProtocol :: Protocol
semigroupProtocol =
  Protocol
    "Semigroup"
    (TE.KindedType (
      TE.Lambda [var] (TE.Record
        [("++", tvar `TE.Arrow` (tvar `TE.Arrow` tvar))
        ])
      )
      K.Type
    )
    [ P.Implementation ( semigroupID
                       , Forall [polyvar'] (Map.empty :| [] :=> T.Applied listConstructor (T.Var polyvar'))
                       , E.Record [("++", E.Identifier "$list_a_$semigroup_$append")]
                       )
    ]
      where
        var = "t"
        tvar = TE.Identifier var
        polyvar = "a"
        polyvar' = T.Poly polyvar K.Type


eqID, numID, isStringID, functorID, semigroupID :: P.Name
eqID = P.Name "__core.eq"
numID = P.Name "__core.num"
isStringID = P.Name "__core.isString"
functorID = P.Name "__core.functor"
semigroupID = P.Name "__core.semigroup"




builtInFns :: Map.Map String (Polymorphic Type)
builtInFns =
  Map.fromList
    [ ("+", binaryNumTypeExpr)
    --, (".", fieldAccessTypeExpr)
    , ("-", binaryNumTypeExpr)
    , ("*", binaryNumTypeExpr)
    , ("/", div)
    , ("++", appendFn)
    , ("==", binaryEqTypeExpr)
    , ("map", mapImplType)
    ]
    where
      var = "a"
      tvar = T.Poly var K.Type

      nonZeroNum = T.Local "NonZero" K.Type
      clause = Map.fromList [(nonZeroNum, Q.none :=> T.Var tvar)]
      liquidBindings = Map.fromList [("x", T.Var nonZeroNum)]
      refinement = L.Negation $ L.Equality (L.Number 0) (L.Var "x")
      div =  Forall [tvar] $ clause
              :| [T.Var tvar `Q.Implements` "Num", Q.Refinement liquidBindings refinement (T.Var nonZeroNum)  ]
              :=> T.Var tvar `T.Arrow` (T.Var nonZeroNum `T.Arrow` T.Var tvar)

      binaryEqTypeExpr  = Forall [tvar] $ Map.empty :| [T.Var tvar `Q.Implements` "Eq"]          :=> T.Var tvar `T.Arrow` (T.Var tvar `T.Arrow` bool)
      binaryNumTypeExpr = Forall [tvar] $ Map.empty :| [T.Var tvar `Q.Implements` "Num"]         :=> T.Var tvar `T.Arrow` (T.Var tvar `T.Arrow` T.Var tvar)
      appendFn          = Forall [tvar] $ Map.empty :| [T.Var tvar `Q.Implements` "Semigroup"]   :=> T.Var tvar `T.Arrow` (T.Var tvar `T.Arrow` T.Var tvar)

mapImplType :: Polymorphic Type
mapImplType = Forall [tf, ta, tb] $ Map.empty :| [T.Var tf `Q.Implements` "Functor"] :=> (fn `T.Arrow` (fa `T.Arrow` fb ))
  where
    f = "f"
    a = "a"
    b = "b"
    tf = T.Poly f (K.Arrow K.Type K.Type)
    ta = T.Poly a K.Type
    tb = T.Poly b K.Type
    fn = T.Var ta `T.Arrow` T.Var tb
    fa = T.Applied (T.Var tf) (T.Var ta)
    fb = T.Applied (T.Var tf) (T.Var tb)




defaultEnv :: CompilerState
defaultEnv = Saga
  { values = Map.empty
  , types = builtInTypes <> builtInFns
  , kinds = Map.empty
  , dataTypes = Map.empty
  , tags = []
  , assumptions = []
  , protocols =
      [ eqProtocol, numProtocol, isStringProtocol
      , functorProtocol, semigroupProtocol
      ]
  }



builtInTypes :: Map.Map String (Polymorphic Type)
builtInTypes = Map.fromList
  [ ("Int",       Forall [] $ Map.empty :| [] :=> int)
  , ("Bool",      Forall [] $ Map.empty :| [] :=> bool)
  , ("String",    Forall [] $ Map.empty :| [] :=> string)
  , ("List",      Forall [] $ Map.empty :| [] :=> listConstructor)
  , ("Function",  Forall [] $ Map.empty :| [] :=> fnConstructor)
  , ("Record",    Forall [] $ Map.empty :| [] :=> T.Record [])
  ]
