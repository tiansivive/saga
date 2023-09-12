module REPL.Repl where


import qualified Saga.Language.Evaluation                          as E
import qualified Saga.Language.TypeSystem.HindleyMilner.Check      as HMC

import qualified Saga.Language.TypeSystem.HindleyMilner.Inference  as HMI
import qualified Saga.Language.TypeSystem.HindleyMilner.Refinement as HMR

import qualified Saga.Parser.Parser                                as P

import           Data.Map                                          (Map)
import qualified Data.Map                                          as Map

import           Control.Monad.Except                              (ExceptT)
import           Control.Monad.State                               (StateT)
import           Debug.Trace                                       (traceM)
import           Saga.Language.Core.Syntax                         (Expr)
import           Saga.Parser.Desugar
import           Saga.Parser.ParsingInfo                           hiding (Term)
import           Saga.Parser.Shared                                (ParsedData (..))
import           System.Console.Haskeline                          (defaultSettings,
                                                                    getInputLine,
                                                                    outputStrLn,
                                                                    runInputT)
import           Text.Pretty.Simple                                (pPrint)


data Sort = Type | Kind | Term

data SagaCmd
    = Parse Sort String
    | Infer Sort String
    | Let String String
    | TypeCheck String String
    | Env
    | Quit
    | Help
    | None

type Env = Map String E.Value
type REPL = StateT Env (ExceptT String IO)

repl :: IO ()
repl = runInputT defaultSettings $ repl' Map.empty
    where
        trim = unwords . words

        getCmd (':' : 'l' : 'e' : 't' : ' ' : binding)
            | (var, expr) <- break (== '=') binding = Let (trim var) (tail expr)

        getCmd (':' : 't' : ' ' : ty) = Infer Type ty
        getCmd (':' : 'k' : ' ' : k) = Infer Kind k

        getCmd (':' : 'p' : 'e' : ' ' : e) = Parse Term e
        getCmd (':' : 'p' : 't' : ' ' : ty) = Parse Type ty
        getCmd (':' : 'p' : 'k' : ' ' : k) = Parse Kind k

        getCmd input | [":check", expr, ty] <- words input = TypeCheck expr ty

        getCmd ":env" = Env
        getCmd ":h"   = Help
        getCmd ":q"   = Quit
        getCmd _      = None

        repl' env = do
            (Just line) <- getInputLine "Saga λ> "
            case getCmd line of
                Quit -> return ()
                Let var expr -> evalDec var expr
                None -> evalExpr line
                Env -> print' env
                Infer Type ty -> inferType ty
                -- Infer Kind k -> inferKind k
                Parse Term e -> parseTerm e
                Parse Type ty -> parseType ty
                Parse Kind k -> parseKind k
                TypeCheck expr ty -> do
                    typecheck expr ty
                    repl' env
                _    -> do
                    outputStrLn "Not implemented yet!"
                    repl' env


            where

                desugaredExpr = (fmap . fmap) desugarExpr
                desugaredTyExpr = (fmap . fmap) desugarTypeExpr
                evalExpr line = case result of
                    Left e -> do { pPrint e; repl' env }
                    Right val -> do
                        pPrint val
                        repl' env
                    where
                        result = do
                            Parsed expr _ _ <- desugaredExpr $ P.runSagaExpr line
                            E.run (Just env) (E.eval expr)

                evalDec var expr = case result of
                    Left e -> do { pPrint e; repl' env }
                    Right val -> do
                        pPrint val
                        repl' $ Map.insert var val env
                    where
                        result = do
                            Parsed e _ _ <- desugaredExpr $ P.runSagaExpr expr
                            E.run (Just env) (E.eval e)


                typecheck expr ty = let
                        parsed = do
                            Parsed expr' _ _ <- desugaredExpr $ P.runSagaExpr expr
                            Parsed tyExpr' _ _ <- desugaredTyExpr $ P.runSagaType ty
                            ty' <- HMR.run tyExpr'
                            (bool, constraints) <- HMC.run expr' ty'
                            return (expr', ty', bool, constraints)
                    in case parsed of
                        Left e -> pPrint e
                        Right (expr', ty', bool, constraints) -> do
                            outputStrLn "\nExpression:"
                            pPrint expr'
                            outputStrLn "\nType:"
                            pPrint ty'
                            outputStrLn "\nConstraints:"
                            pPrint constraints
                            outputStrLn "\nTypecheck:"
                            pPrint bool

                parseTerm input = do
                    case P.runSagaExpr input of
                        (Left e)  -> pPrint e
                        (Right t) -> pPrint t
                    repl' env

                parseType input = do
                    case P.runSagaType input of
                        (Left e)   -> pPrint e
                        (Right ty) -> pPrint ty
                    repl' env

                parseKind input = do
                    case P.runSagaKind input of
                        (Left e)   -> pPrint e
                        (Right ty) -> pPrint ty
                    repl' env

                inferType input = do
                    case HMI.run input of
                        (Left e)   -> pPrint e
                        (Right ty) -> pPrint ty
                    repl' env

                print' env' = do
                    outputStrLn "-----------\nEnv:"
                    pPrint env'
                    outputStrLn "-----------"
                    repl' env

                -- inferKind input = do
                --     case Infer.inferKind input of
                --         (Left e)  -> pPrint e
                --         (Right k) -> pPrint k
                --     repl' env






-- script :: FilePath -> IO ()
-- script fp =
--     let
--         check' (Let _ tyExpr kind expr) = case (tyExpr, kind) of
--             (Just tyExpr', Just kind') -> do
--                 check_kind tyExpr' kind'
--                 inferred <- Infer.typeof expr
--                 ty <- Infer.reduce tyExpr'
--                 matches <- check expr tyExpr'
--                 update id inferred ty matches

--             (Just tyExpr', Nothing) -> do
--                 k <- Infer.kindOf tyExpr'
--                 check_kind tyExpr' k
--                 inferred <- Infer.typeof expr
--                 ty <- Infer.reduce tyExpr'
--                 matches <- check expr tyExpr'
--                 update id inferred ty matches

--             (Nothing, Just kind') -> do
--                 inferred <- Infer.typeof expr
--                 let tyExpr' = T.Type inferred
--                 check_kind tyExpr' kind'
--                 matches <- check expr tyExpr'
--                 update id inferred inferred matches

--             (Nothing, Nothing) -> do
--                 inferred <- Infer.typeof expr
--                 let tyExpr' = T.Type inferred
--                 k <- Infer.kindOf tyExpr'
--                 check_kind tyExpr' k
--                 matches <- check expr tyExpr'
--                 update id inferred inferred matches

--         check' (Scripts.Type (Language.Name _ id) kind tyExpr) = do
--             k <- Infer.kindOf tyExpr
--             ty <- Infer.reduce tyExpr
--             matches <- check_kind tyExpr k
--             if not matches
--                 then throwError $ Infer.WrongKind ty k
--                 else do
--                     modify $ \s -> s{ Infer.typeVars = Map.insert id ty $ Infer.typeVars s }
--                     return matches



--         update :: String -> T.Type a -> T.Type a -> Bool -> Infer.Infer a Bool
--         update id inferred ty matches = if not matches
--             then throwError $ Infer.TypeMismatch inferred ty
--             else do
--                 modify $ \s -> s{ Infer.expressions = Map.insert id ty $ Infer.expressions s }
--                 return matches

--         typecheck' str = do
--             (Scripts.Script _ _ decs _) <- runSagaScript str
--             results <- Infer.doInEnv Nothing $ mapM check' decs
--             return $  and `first` results
--         eval' str = do
--             (Scripts.Script _ _ decs _) <- runSagaScript str
--             runStateT (mapM E.evalDeclaration decs) Map.empty


--     in do
--         handle <- openFile fp ReadMode
--         evalH <- openFile "./lang/evaluation.log" WriteMode
--         tcH <- openFile "./lang/typecheck.log" WriteMode

--         contents <- hGetContents handle
--         case typecheck' contents of
--             Left e -> putStrLn e
--             Right val -> do
--                 case val of
--                     (False, env) -> do
--                         putStrLn "Found errors!"
--                         pPrint env
--                         pHPrint tcH env
--                     (True, env) -> do
--                         putStrLn "No errors found!"
--                         pPrint env
--                         pHPrint tcH env
--                         case eval' contents of
--                             Left e           -> putStrLn e
--                             Right (val, env) -> do
--                                 -- putStrLn "Value:"
--                                 -- pPrint val
--                                 -- putStrLn "\n\nEnv:"
--                                 -- pPrint env
--                                 pHPrint evalH val
--                                 pHPrint evalH "\n\n------------------------------------\n------------------------------------\n\nEnv:\n"
--                                 pHPrint evalH env


--         hClose handle
--         hClose evalH
--         hClose tcH
--         putStrLn "Bye!"
