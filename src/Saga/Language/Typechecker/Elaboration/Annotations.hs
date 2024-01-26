module Saga.Language.Typechecker.Elaboration.Annotations where
import qualified Saga.Language.Syntax.Elaborated.AST   as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds as K
import qualified Saga.Language.Syntax.Elaborated.Types as T


decorate ty@(T.Singleton {}) = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Tuple {})     = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Record {})    = EL.Annotated ty $ EL.Raw K.Type
