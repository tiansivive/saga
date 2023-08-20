{-# LANGUAGE MultiParamTypeClasses #-}

module Saga.AST.TypeSystem.HindleyMilner.Environment where

import           Control.Monad.Except
import           Control.Monad.State.Lazy                hiding (get, modify)
import qualified Control.Monad.State.Lazy                as ST
import           Control.Monad.Trans.RWS                 (RWST, get, local,
                                                          modify, tell)
import qualified Data.Map                                as Map
import           Prelude                                 hiding (EQ)
import qualified Saga.AST.TypeSystem.HindleyMilner.Types as T
import           Saga.AST.TypeSystem.HindleyMilner.Types hiding (Implements,
                                                          ProtocolID)
import           Saga.Lexer.Tokens                       (Token (Qualified))

type UnificationVar = Tyvar

type TypeVar = String
type Alias = String

type Name = String

type BaseProtocol = String

type ProtocolID = String

data Scheme = Scheme [Tyvar] (Qualified Type) deriving (Show, Eq)

data Protocol = Protocol {id :: ProtocolID, spec :: [Method], supers :: [BaseProtocol], implementations :: [Implementation]}
  deriving (Show)

type Implementation = Qualified ImplConstraint

type Method = (Name, Type)

builtInProtocols :: Map.Map ProtocolID Protocol
builtInProtocols = Map.fromList [("Num", numProtocol), ("IsString", isStringProtocol)]

numProtocol :: Protocol
numProtocol =
  Protocol
    "Num"
    [("+", tyvar`TArrow` tyvar `TArrow` tyvar)]
    []
    [ [] :=> (TPrimitive TInt `IP` "Num")
    ]
    where tyvar = TVar $ Tyvar "a" KType

isStringProtocol :: Protocol
isStringProtocol =
  Protocol
    "IsString"
    [("isString", tyvar`TArrow` TPrimitive TBool)]
    []
    [ [] :=> TPrimitive TString `IP` "IsString"
    ]
    where tyvar = TVar $ Tyvar "a" KType

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

data InferenceEnv = Env
  { unificationVars :: Map.Map UnificationVar Scheme,
    aliases         :: Map.Map Alias Scheme
  }
  deriving (Show)

newtype InferenceState = IST {count :: Int}

type Infer = RWST InferenceEnv [IConstraint] InferenceState (Except InferenceError)

data IConstraint
  = Empty
  | EqCons Equality
  | ImplCons ImplConstraint
      -- \| Subtype Type Type -- Is this at all needed? probably not
      -- | Conjunction IConstraint IConstraint
      -- | Implication [UnificationVar] IConstraint IConstraint
  deriving (Show, Eq)

data Equality = EQ Type Type deriving (Show, Eq)
data ImplConstraint = IP Type ProtocolID deriving (Show, Eq)

data InferenceError
  = UnboundVariable String
  | UndefinedIdentifier Alias
  | UnexpectedType String
  | UnificationMismatch [Type] [Type]
  | UnificationFail Type Type
  | InfiniteType UnificationVar Type
  | SubtypeFailure Type Type
  | Fail String
  deriving (Show, Eq)

emit :: IConstraint -> Infer ()
emit = tell . pure

empty :: InferenceEnv
empty = Env Map.empty builtInFns

initState :: InferenceState
initState = IST {count = 0}



builtInFns :: Map.Map Alias Scheme
builtInFns =
  Map.fromList
    [ ("+", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar))),
      ("-", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar))),
      ("*", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar))),
      ("/", Scheme [var] ([tvar `T.Implements` "Num"] :=> tvar `TArrow` (tvar `TArrow` tvar)))
    ]
    where
      var = Tyvar "a" KType
      tvar = TVar var


union :: InferenceEnv -> InferenceEnv -> InferenceEnv
(Env unifier aliases) `union` (Env unifier' aliases') =
  Env
    { unificationVars = Map.union unifier unifier',
      aliases = Map.union aliases aliases'
    }

extend :: InferenceEnv -> (UnificationVar, Scheme) -> InferenceEnv
extend e@(Env unifier aliases) (var, scheme) = e {unificationVars = Map.insert var scheme unifier}

scoped :: Infer a -> (UnificationVar, Scheme) -> Infer a
scoped m (var, scheme) = do
  let scoped' = local $ \env -> env `extend` (var, scheme)
  scoped' m

fresh :: Kind -> Infer Type
fresh k = do
  modify $ \s -> s {count = count s + 1}
  s <- get
  let v = "t" ++ show ([1 ..] !! count s)
  return $ TVar $ Tyvar v k

letters :: [String]
letters = [1 ..] >>= flip replicateM ['α' .. 'ω']

mkIConstraint :: Constraint -> IConstraint
mkIConstraint (ty `T.Implements` protocol) = ImplCons $ ty `IP` protocol


implementationConstraints :: [IConstraint] -> [ImplConstraint]
implementationConstraints cs = [ ip | ImplCons ip <- cs ]

implementationTy :: ImplConstraint -> Type
implementationTy (ty `IP` p) = ty

implementationP :: ImplConstraint -> String
implementationP (ty `IP` p) = p
