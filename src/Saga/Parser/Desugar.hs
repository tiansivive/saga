module Saga.Parser.Desugar where

import           Debug.Trace                                  (trace)
import           Unsafe.Coerce                                (unsafeCoerce)

import           Data.Functor                                 ((<&>))
import qualified Saga.Language.Core.Literals                  as Core
import qualified Saga.Language.Core.Syntax                    as Core
import qualified Saga.Language.TypeSystem.Types as CoreTy
import qualified Saga.Parser.Expr                             as Parser
import qualified Saga.Parser.Types                            as ParserTy



desugarExpr :: Parser.Expr -> Core.Expr
desugarExpr (Parser.Literal lit) = Core.Literal lit
desugarExpr (Parser.Identifier id) = Core.Identifier id
desugarExpr (Parser.Hole id) = Core.Hole id
desugarExpr (Parser.List list) = Core.List $ fmap desugarExpr list
desugarExpr (Parser.Tuple tuple) = Core.Tuple $ fmap desugarExpr tuple
desugarExpr (Parser.Record pairs) = Core.Record $ fmap desugarExpr <$> pairs
desugarExpr (Parser.IfElse cond true false) = Core.Match (desugarExpr cond) [Core.Case true' (desugarExpr true), Core.Case false' (desugarExpr false) ]
desugarExpr (Parser.Match expr cases) = Core.Match (desugarExpr expr) (fmap desugarCase cases)
desugarExpr (Parser.Clause expr bindings) = desugarExpr $ Parser.Block $ stmts ++ [return']
    where
        return' = Parser.Return expr
        stmts = bindings <&> \(Parser.Bind id e) -> Parser.Declaration $ Parser.Let id Nothing Nothing e

desugarExpr (Parser.FnApp expr args) = Core.FnApp (desugarExpr expr) (fmap desugarExpr args)
-- | we fold from the left and wrap each subsequent pattern in a lambda, inverting arg order. We correct by reversion here
desugarExpr (Parser.Lambda params body) = foldrec match' 1 (desugarExpr body) (reverse params)
    where
        match' :: Int -> Core.Expr -> Parser.Pattern -> Core.Expr
        match' n e pat = case pat of
            Parser.Id id        -> Core.Lambda [id] e
            Parser.PatHole id   -> Core.Lambda [id] e
            pat                 -> Core.Lambda [id] (Core.Match (Core.Identifier id) [toCase pat])
            where
                id = "_var" ++ show n
                toCase p' = Core.Case (desugarPattern p') e

        foldrec :: (Int -> b -> a -> b) -> Int -> b -> [a] ->  b
        foldrec foldFn n current [] = current
        foldrec foldFn n current (x: xs) = foldrec foldFn (n + 1) (foldFn n current x) xs

desugarExpr (Parser.Block stmts) = go [] stmts
    where
        go done [] = Core.Block done
        go done (stmt: rest) = go (done ++ [next]) rest'
            where
                (next, rest') = desugarStmt stmt
                desugarStmt (Parser.Return expr)        = (Core.Return $ desugarExpr expr, [])
                desugarStmt (Parser.Procedure expr)     = (Core.Procedure $ desugarExpr expr , rest)
                desugarStmt (Parser.Declaration dec)    = (Core.Declaration $ desugarDec dec, rest)
                desugarStmt (Parser.BackCall pats expr) = desugarStmt stmt'
                    where
                        stmt' = Parser.Return $ Parser.FnApp expr [Parser.Lambda pats (Parser.Block rest)]


desugarCase :: Parser.Case -> Core.Case
desugarCase (Parser.Case pat e) = Core.Case (desugarPattern pat) (desugarExpr e)

desugarPattern ::  Parser.Pattern -> Core.Pattern
desugarPattern Parser.Wildcard         = Core.Wildcard
desugarPattern (Parser.Id id)          = Core.Id id
desugarPattern (Parser.PatHole id)     = Core.PatHole id
desugarPattern (Parser.Lit lit)        = Core.Lit lit
desugarPattern (Parser.PatData tag pats) = Core.PatData tag (fmap desugarPattern pats)
desugarPattern (Parser.PatTuple tuple rest) = Core.PatTuple (fmap desugarPattern tuple) rest
desugarPattern (Parser.PatList list rest) = Core.PatList (fmap desugarPattern list) rest
desugarPattern (Parser.PatRecord pairs rest) = Core.PatRecord (fmap (fmap desugarPattern) <$> pairs) rest


desugarDec :: Parser.Declaration -> Core.Declaration
desugarDec (Parser.Let id ty kind e) = Core.Let id (fmap desugarTypeExpr ty) (fmap desugarKind kind) (desugarExpr e)
desugarDec (Parser.Type id kind ty) = Core.Type id (fmap desugarKind kind) (desugarTypeExpr ty)
desugarDec (Parser.Data id kind dataExprs bindings) = Core.Data id (fmap desugarKind kind) (fmap desugarDataExpr dataExprs) (fmap desugarTyBinding bindings)


desugarScript :: Parser.Script -> Core.Script
desugarScript (Parser.Script decs) = Core.Script (fmap desugarDec decs)

desugarTypeExpr :: ParserTy.TypeExpr -> CoreTy.TypeExpr
desugarTypeExpr (ParserTy.TIdentifier id)                   = CoreTy.TIdentifier id
desugarTypeExpr (ParserTy.TLiteral lit)                     = CoreTy.TAtom $ CoreTy.TLiteral lit
desugarTypeExpr (ParserTy.TETuple t)                        = CoreTy.TComposite $ CoreTy.TETuple (fmap desugarTypeExpr t)
desugarTypeExpr (ParserTy.TERecord pairs)                   = CoreTy.TComposite $ CoreTy.TERecord (fmap desugarTypeExpr <$> pairs)
desugarTypeExpr (ParserTy.TEArrow input out)                = CoreTy.TComposite $ CoreTy.TEArrow (desugarTypeExpr input) (desugarTypeExpr out)
desugarTypeExpr (ParserTy.TEUnion union)                    = CoreTy.TComposite $ CoreTy.TEUnion (fmap desugarTypeExpr union)
desugarTypeExpr (ParserTy.TConditional cond true false)     = CoreTy.TConditional (desugarTypeExpr cond)  (desugarTypeExpr true)  (desugarTypeExpr false)
desugarTypeExpr (ParserTy.TTagged tag tyExpr)               = CoreTy.TTagged tag (desugarTypeExpr tyExpr)
desugarTypeExpr (ParserTy.TClause tyExpr bindings)          = CoreTy.TClause (desugarTypeExpr tyExpr) (fmap desugarTyBinding bindings)
desugarTypeExpr (ParserTy.TImplementation protocol tyExpr)  = CoreTy.TImplementation protocol (desugarTypeExpr tyExpr)
desugarTypeExpr (ParserTy.TLambda params tyExpr)            = CoreTy.TLambda params (desugarTypeExpr tyExpr)
desugarTypeExpr (ParserTy.TFnApp tyFn tyArgs)               = CoreTy.TFnApp (desugarTypeExpr tyFn) (fmap desugarTypeExpr tyArgs)


desugarKind :: ParserTy.Kind -> CoreTy.Kind
desugarKind ParserTy.KType = CoreTy.KType
desugarKind (ParserTy.KProtocol k) = CoreTy.KProtocol $ desugarKind k
desugarKind (ParserTy.KVar id) = CoreTy.KVar id
desugarKind (ParserTy.KArrow inK outK) = CoreTy.KArrow (desugarKind inK) (desugarKind outK)


desugarDataExpr:: Parser.DataExpr -> Core.DataExpr
desugarDataExpr = fmap desugarTypeExpr

desugarTyBinding :: ParserTy.Binding ParserTy.TypeExpr -> CoreTy.Binding CoreTy.TypeExpr
desugarTyBinding (ParserTy.Bind id tyExpr) = CoreTy.Bind id (desugarTypeExpr tyExpr)
desugarTyBinding (ParserTy.ImplBind id protocol) = CoreTy.ImplBind (desugarTypeExpr id) protocol
desugarTyBinding (ParserTy.SubtypeBind id tyExpr) = CoreTy.SubtypeBind id (desugarTypeExpr tyExpr)
desugarTyBinding (ParserTy.RefineBind id tyExpr) = CoreTy.RefineBind id (desugarTypeExpr tyExpr)

true', false' :: Core.Pattern
true' = Core.Lit (Core.LBool True)
false' = Core.Lit (Core.LBool False)
