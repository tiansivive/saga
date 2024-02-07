module Saga.Language.Typechecker.Test where



import qualified Saga.Language.Syntax.Elaborated.Kinds as EK
import qualified Saga.Language.Syntax.Elaborated.Types as EL

import qualified Saga.Language.Syntax.Elaborated.AST   as AST
import           Saga.Language.Syntax.Literals         (Literal (..))
import qualified Saga.Language.Syntax.Reduced.AST      as RD
import qualified Saga.Language.Syntax.Reduced.Values   as RD




import qualified Saga.Language.Typechecker.Run         as Run

import           Prelude                               hiding (print)
import           Text.Pretty.Simple                    (pPrint)




fn = RD.Raw $
    RD.Lambda ["x"] $
        RD.Raw (RD.Application
            (RD.Raw (RD.Var "x"))
            [RD.Raw (RD.Literal $ LInt 1)]
            )


tvar = EL.Var
        ( EL.Poly "α" ( EK.Var $ EK.Poly "αk" ))



test = Run.typecheck fn >>= print


print (Left fail) = do
    putStrLn "FAILURE:"
    pPrint fail
    return ()
print (Right (Left (callstack, err))) = do
    putStrLn "\n---------------------\nSAGA ERROR:\n"
    pPrint err
    putStrLn "\n---------------------\nCallstack:\n"
    pPrint callstack
    return ()
print (Right (Right res)) = do
    let ((ast, constraint, state, residuals, solution, cycles, zonked, ty), info) = res
    putStrLn "\n--------------------- INFO --------------------- "
    pPrint info
    putStrLn "\n--------------------- AST --------------------- "
    pPrint ast
    putStrLn "\n--------------------- CONSTRAINT --------------------- "
    pPrint constraint
    putStrLn "\n--------------------- STATE --------------------- "
    pPrint state
    putStrLn "\n--------------------- RESIDUALS --------------------- "
    pPrint residuals
    putStrLn "\n--------------------- CYCLES --------------------- "
    pPrint cycles
    putStrLn "\n--------------------- SOLUTION --------------------- "
    pPrint solution
    putStrLn "\n--------------------- ZONKED --------------------- "
    pPrint zonked
    putStrLn "\n--------------------- QUALIFIED TYPE --------------------- "
    pPrint ty

    return ()





-- int = E.Literal . LInt

-- str = E.Literal . LString


-- gt x y = E.FnApp (E.Identifier ">") [x, y]
-- gte x y = E.FnApp (E.Identifier ">=") [x, y]
-- lt x y = E.FnApp (E.Identifier "<") [x, y]
-- lte x y = E.FnApp (E.Identifier "<=") [x, y]


-- op x = E.FnApp (E.Identifier "/") [E.Literal $ LInt 1, x]

-- cases = E.Lambda ["x"] $
--         E.Match (E.Identifier "x")
--             [ E.Case (E.Lit $ LInt 1) (E.Identifier "x")
--             , E.Case (E.Lit $ LString "Hello") (E.Literal $ LString "World")
--             ]

-- hofType = T.Forall [T.Poly "a" K.Type] $ Q.none :=> (T.Data "Int" K.Type `T.Arrow` T.Data "Int" K.Type) `T.Arrow` T.Data "Int" K.Type
-- hofTypeExpr = (TE.Identifier "Int" `TE.Arrow` TE.Identifier "Int") `TE.Arrow` TE.Identifier "Int"
-- hof = E.Lambda ["f"] $ E.FnApp (E.Identifier "f") [E.Literal $ LInt 1]

-- dec = Let "hof" (Just hofTypeExpr) Nothing hof


-- tc d = run $ typecheck d

-- test e = run $ do
--     ((e, st), cs) <- TI.run e
--     Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState @[Cycle Type] [] . Eff.runReader (Var.Level 0) . Eff.runReader (levels st) $ simplified cs
--         where
--             simplified = mapM simplify . Shared.flatten


