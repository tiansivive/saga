module Saga.Language.Typechecker.Test where



import qualified Saga.Language.Syntax.Elaborated.Kinds as EK
import qualified Saga.Language.Syntax.Elaborated.Types as ET

import qualified Saga.Language.Syntax.Elaborated.AST   as EL
import           Saga.Language.Syntax.Literals         (Literal (..))
import qualified Saga.Language.Syntax.Reduced.AST      as RD
import qualified Saga.Language.Syntax.Reduced.Types    as RT
import qualified Saga.Language.Syntax.Reduced.Values   as RD




import qualified Saga.Language.Typechecker.Run         as Run

import           Control.Monad                         ((>=>))
import           Prelude                               hiding (print)
import           Saga.Language.Syntax.Polymorphism     (Polymorphic (..))
import           Text.Pretty.Simple                    (pPrint)






test = Run.typecheck >=> print


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


fn = RD.Raw $
    RD.Lambda ["x"] $
        RD.Raw (RD.Application
            (RD.Raw (RD.Var "x"))
            [RD.Raw (RD.Literal $ LInt 1)]
            )


tvar = ET.Var
        ( ET.Poly "α" ( EK.Var $ EK.Poly "αk" ))




int = RD.Literal . LInt

str = RD.Literal . LString


gt x y = RD.Application (RD.Raw $ RD.Var ">") [x, y]
gte x y = RD.Application (RD.Raw $ RD.Var ">=") [x, y]
lt x y = RD.Application (RD.Raw $ RD.Var "<") [x, y]
lte x y = RD.Application (RD.Raw $ RD.Var "<=") [x, y]


divide x = RD.Raw $ RD.Application (RD.Raw $ RD.Var "/") [RD.Raw $ int 1, RD.Raw $ int x]

cases = RD.Lambda ["x"] $
            RD.Raw (RD.Match (RD.Raw $ RD.Var "x")
                [ RD.Raw $ RD.Case (RD.Raw . RD.PatLit $ LInt 1) (RD.Raw $ RD.Var "x")
                , RD.Raw $ RD.Case (RD.Raw . RD.PatLit $ LString "Hello") (RD.Raw $ str "World")
                ])

-- hofType = RT.Polymorphic ["a"] $ (RT.Identifier "a" `RT.Arrow` RT.Identifier "a") `RT.Arrow` RT.Identifier "a"

--ET.Data "Int" K.Type `T.Arrow` T.Data "Int" K.Type) `T.Arrow` T.Data "Int" K.Type
hofTypeExpr = RT.Lambda ["a"] $ (RT.Identifier "a" `RT.Arrow` RT.Identifier "a") `RT.Arrow` RT.Identifier "a"
hof = RD.Lambda ["f"] $ RD.Raw (RD.Application (RD.Raw $ RD.Var "f") [RD.Raw $ int 1])

dec = RD.Let "hof" (RD.Annotated hof (RD.Raw hofTypeExpr))


-- tc d = run $ typecheck d

-- test e = run $ do
--     ((e, st), cs) <- TI.run e
--     Eff.runState initialSolution . Eff.evalState initialCount .  Eff.runState @[Cycle Type] [] . Eff.runReader (Var.Level 0) . Eff.runReader (levels st) $ simplified cs
--         where
--             simplified = mapM simplify . Shared.flatten


