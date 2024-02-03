{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Saga.Language.Typechecker.Zonking.Monad where


import qualified Effectful.Reader.Static                       as Eff

import qualified Data.Kind                                     as GHC
import           Effectful                                     (Eff, (:>))
import qualified Effectful                                     as Eff
import qualified Effectful.Error.Static                        as Eff
import qualified Effectful.Fail                                as Eff
import qualified Effectful.Writer.Static.Local                 as Eff
import           Saga.Language.Syntax.AST                      (NodeType,
                                                                Phase (..))
import           Saga.Language.Syntax.Elaborated.AST
import           Saga.Language.Typechecker.Env                 (CompilerState,
                                                                Info)
import           Saga.Language.Typechecker.Errors              (SagaError)
import           Saga.Language.Typechecker.Solving.Constraints (Constraint)
import           Saga.Language.Typechecker.Solving.Monad       (Solution)


type Zonking es =   ( Eff.Reader (CompilerState Zonked) :> es
                    , Eff.Writer Info :> es
                    , Eff.Error SagaError :> es
                    , Eff.Fail :> es

                    , Eff.Reader Context :> es
                    )

data Context = Context { solution:: Solution, residuals :: [Constraint] } deriving Show

class Zonk (nt :: NodeType) where
    type family Effects nt (es :: [Eff.Effect]) :: GHC.Constraint
    zonk :: Effects nt es => AST Elaborated nt -> Eff es (AST Zonked nt)
    -- lookup :: Effects nt es => Tag a -> Variable a -> Eff es (Maybe a)

