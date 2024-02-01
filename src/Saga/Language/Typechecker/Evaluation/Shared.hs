module Saga.Language.Typechecker.Evaluation.Shared where
import qualified Saga.Language.Syntax.Reduced.AST as E



node (E.Annotated node _) = node
node (E.Raw node)         = node

annotation (E.Annotated _ ann) = ann
annotation (E.Raw node)        = error $ "Tried to extract annotation from raw node: " ++ show node
