module Saga.Language.TypeSystem.Lib where

import           Data.Functor                                       ((<&>))
import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.Environment
import qualified Saga.Language.TypeSystem.Types       as T
import           Saga.Language.TypeSystem.Types

eqProtocol :: Protocol
eqProtocol =
  Protocol
    "Eq"
    (TLambda [param] (TComposite (TERecord
      [("==", TComposite $ param' `TEArrow` TComposite (param' `TEArrow` TAtom (TPrimitive TBool)))
      ])
    ))
    []
    [ [] :=> TPrimitive TInt `IP` "Eq"
    , [] :=> TPrimitive TString `IP` "Eq"
    , [] :=> TPrimitive TBool `IP` "Eq"
    ]
    where
      param = "a"
      param' = TIdentifier param

numProtocol :: Protocol
numProtocol =
  Protocol
    "Num"
    (TLambda [param] (TComposite (TERecord
      [("+", TComposite $ param' `TEArrow` TComposite (param' `TEArrow` param'))
      ,("-", TComposite $ param' `TEArrow` TComposite (param' `TEArrow` param'))
      ,("*", TComposite $ param' `TEArrow` TComposite (param' `TEArrow` param'))
      ,("/", TComposite $ param' `TEArrow` TComposite (param' `TEArrow` param'))
      ])
    ))
    []
    [ [] :=> TPrimitive TInt `IP` "Num"
    ]
    where
      param = "a"
      param' = TIdentifier param

isStringProtocol :: Protocol
isStringProtocol =
  Protocol
    "IsString"
    (TLambda [param] (TComposite (TERecord
      [("isString", TComposite $ param' `TEArrow` TAtom (TPrimitive TBool))
      ])
    ))
    []
    [ [] :=> TPrimitive TString `IP` "IsString"
    ]
    where
      param = "a"
      param' = TIdentifier param


functorProtocol :: Protocol
functorProtocol =
  Protocol
    "Functor"
    (TLambda [functor] (TComposite (TERecord
      [("map", TLambda [a, b] $ TComposite $ fn `TEArrow` TComposite (fa `TEArrow` fb))
      ])
    ))
    []
    [ [] :=> listConstructor `IP` "Functor"
    ]
      where
        functor = "f"
        a = "a"
        b = "b"
        var = TIdentifier
        fn = TComposite (var a `TEArrow` var b)
        fa = TFnApp (var functor) [var a]
        fb = TFnApp (var functor) [var b]

semigroupProtocol :: Protocol
semigroupProtocol =
  Protocol
    "Semigroup"
    (TLambda [param] (TComposite (TERecord
      [("++", TComposite $ param' `TEArrow` TComposite (param' `TEArrow` param'))
      ])
    ))
    []
    [ [] :=> TApplied listConstructor (TVar $ Tyvar param KType) `IP` "Semigroup"
    ]
      where
        param  = "a"
        param' = TIdentifier param


numProtocols :: [ProtocolID]
numProtocols =
  [ "Num",
    "Integral",
    "Floating",
    "Fractional",
    "Real",
    "RealFloat",
    "RealFrac"
  ]

stdProtocols :: [ProtocolID]
stdProtocols =
  [ "Eq",
    "Ord",
    "Show",
    "Read",
    "Bounded",
    "Enum",
    "Ix",
    "Functor",
    "Monad",
    "MonadPlus"
  ]
    ++ numProtocols


builtInTypes :: Map.Map String TypeExpr
builtInTypes = Map.fromList
  [ ("Int", TAtom $ TPrimitive TInt)
  , ("Bool", TAtom $ TPrimitive TBool)
  , ("String", TAtom $ TPrimitive TString)
  , ("List", TAtom listConstructor)
  , ("Function", TAtom fnConstructor)
  ]

listConstructor, fnConstructor :: Type
listConstructor = TData $ Tycon "List" (KArrow KType KType)
fnConstructor = TData $ Tycon "Function" (KArrow KType (KArrow KType KType))


builtInFns :: Map.Map Alias TypeExpr
builtInFns =
  Map.fromList
    [ ("+", binaryNumTypeExpr),
      ("-", binaryNumTypeExpr),
      ("*", binaryNumTypeExpr),
      ("/", binaryNumTypeExpr),
      ("++", appendFn),
      ("==", binaryEqTypeExpr)
    ]
    where
      var = "a"
      tvar = TVar $ Tyvar var KType
      binaryNumTypeExpr = TQualified $ [tvar `T.Implements` "Num"] :=> TLambda [var] (TAtom $ tvar `TArrow` (tvar `TArrow` tvar))
      binaryEqTypeExpr = TQualified $ [tvar `T.Implements` "Eq"] :=> TLambda [var] (TAtom $ tvar `TArrow` (tvar `TArrow` TPrimitive TBool))
      appendFn = TQualified $ [tvar `T.Implements` "Semigroup"] :=> TLambda [var] (TAtom $ tvar `TArrow` (tvar `TArrow` tvar))



defaultEnv :: CompilerState
defaultEnv = Saga { values = builtInFns, types = builtInTypes, kinds = Map.empty, protocols = [eqProtocol, numProtocol, isStringProtocol, functorProtocol, semigroupProtocol] }

startWriter :: Accumulator
startWriter = Acc { logs = [], warnings = [], errors = [] }
