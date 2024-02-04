module Saga.Language.Typechecker.Zonking.Test where

import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import           Saga.Language.Syntax.Polymorphism  (Polymorphic (..))
import qualified Saga.Language.Syntax.Zonked.AST    as Z
import qualified Saga.Language.Syntax.Zonked.Kinds  as ZK
import qualified Saga.Language.Syntax.Zonked.Types  as ZT
import qualified Saga.Language.Syntax.Zonked.Values as Z
import qualified Saga.Language.Typechecker.Shared   as Shared

import           Saga.Language.Syntax.Literals      (Literal (LString))
import           Saga.Utils.Operators               ((||>))

test = Z.Annotated
    (Z.Literal $ LString "Hello, World!" )
    (Z.Raw $ ZT.Polymorphic
                ( Forall
                    [ ZT.Poly "g4" ZK.Type ]
                    (  ZT.Arrow
                                ( Z.Annotated
                                    ( ZT.Var
                                        ( ZT.Poly "g4" ZK.Type )
                                    ) ( Z.Raw ZK.Type )
                                )
                                ( Z.Annotated
                                    ( ZT.Var
                                        ( ZT.Poly "t3"
                                            ( ZK.Var
                                                ( ZK.Poly "k3" )
                                            )
                                        )
                                    )
                                    ( Z.Raw
                                        ( ZK.Var
                                            ( ZK.Poly "k3" )
                                        )
                                    )
                                )

                        )
                    ))

