module Saga.Language.Typechecker.Elaboration.Annotations where
import qualified Saga.Language.Syntax.Elaborated.AST   as EL
import qualified Saga.Language.Syntax.Elaborated.Kinds as K
import qualified Saga.Language.Syntax.Elaborated.Types as T




decorate ty@(T.Singleton {})              = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Tuple {})                  = EL.Annotated ty $ EL.Raw K.Type
decorate ty@(T.Record {})                 = EL.Annotated ty $ EL.Raw K.Type

decorate ty@(T.Var (T.Unification t k))   = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Instantiation t k)) = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Poly t k))          = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Local t k))         = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Rigid t k))         = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Scoped t k))        = EL.Annotated ty $ EL.Raw k
decorate ty@(T.Var (T.Skolem t k))        = EL.Annotated ty $ EL.Raw k

--pattern Raw
