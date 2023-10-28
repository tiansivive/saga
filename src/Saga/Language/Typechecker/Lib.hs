module Saga.Language.Typechecker.Lib where

import qualified Saga.Language.Typechecker.Kind as K
import qualified Saga.Language.Typechecker.Type as T
import           Saga.Language.Typechecker.Type (Type)


listConstructor, fnConstructor :: Type
listConstructor = T.Data "List" (K.Arrow K.Type K.Type)
fnConstructor = T.Data "->" (K.Arrow K.Type (K.Arrow K.Type K.Type))

int, bool, string :: Type
int = T.Data "Int" K.Type
bool = T.Data "Bool" K.Type
string = T.Data "String" K.Type
