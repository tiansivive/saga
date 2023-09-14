module Saga.Language.TypeSystem.HindleyMilner.Lib where

import qualified Data.Map                                           as Map
import           Saga.Language.TypeSystem.HindleyMilner.Environment
import           Saga.Language.TypeSystem.HindleyMilner.Types


builtInProtocols :: Map.Map ProtocolID Protocol
builtInProtocols = Map.fromList [("Num", numProtocol), ("IsString", isStringProtocol)]

numProtocol :: Protocol
numProtocol =
  Protocol
    "Num"
    [("+", TLambda [param] (TAtom $  param' `TArrow` param' `TArrow` param'))]
    []
    [ [] :=> TPrimitive TInt `IP` "Num"
    ]
    where
      param = "a"
      param' = TVar $ Tyvar param KType

isStringProtocol :: Protocol
isStringProtocol =
  Protocol
    "IsString"
    [("isString", TLambda [param] (TAtom $ param' `TArrow` TPrimitive TBool))]
    []
    [ [] :=> TPrimitive TString `IP` "IsString"
    ]
    where
      param = "a"
      param' = TVar $ Tyvar param KType

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
