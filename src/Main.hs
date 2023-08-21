{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Saga.AST.Scripts                              as Scripts
import qualified Saga.AST.Syntax                               as AST
import qualified Saga.Lexer.Lexer                              as L
import           Saga.Parser.Parser                            (runSagaDec,
                                                                runSagaExpr,
                                                                runSagaKind,
                                                                runSagaScript,
                                                                runSagaType)

import qualified Data.Map                                      as Map
import qualified Saga.AST.Evaluation                           as E
import qualified Saga.AST.TypeSystem.HindleyMilner.Check       as HMC
import qualified Saga.AST.TypeSystem.HindleyMilner.Environment as HME
import qualified Saga.AST.TypeSystem.HindleyMilner.Inference   as HMI
import qualified Saga.AST.TypeSystem.HindleyMilner.Refinement  as HMR
import qualified Saga.AST.TypeSystem.Inference                 as Infer
import qualified Saga.Parser.ParserHM                          as HMP
import qualified Saga.Parser.ParsingInfo                       as HMPI

import           Control.Monad.State.Lazy
import           Data.Maybe                                    (fromJust)

import           Control.Monad.Except
import           Control.Monad.RWS                             (evalRWST)
import           Data.Bifunctor                                (first)
import           Saga.AST.TypeSystem.Check                     (check,
                                                                check_kind)
import qualified Saga.AST.TypeSystem.HindleyMilner.Refinement  as HMR
import           Saga.AST.TypeSystem.HindleyMilner.Refinement  (refine)
import           Saga.AST.TypeSystem.HindleyMilner.Types       (Qualified (constraints))
import           Saga.AST.TypeSystem.Inference                 (kindOf)
import qualified Saga.AST.TypeSystem.Types                     as T
import           System.Console.Haskeline                      (defaultSettings,
                                                                getInputLine,
                                                                outputStrLn,
                                                                runInputT)
import           System.IO                                     (IOMode (ReadMode, ReadWriteMode, WriteMode),
                                                                hClose,
                                                                hGetContents,
                                                                openFile)
import           Text.Pretty.Simple                            (pHPrint, pPrint)






main :: IO ()
main = do
    putStrLn "Starting Saga..."
    line <- getLine
    pPrint (runSagaExpr line)

data Sort = Type | Kind | Term

data SagaCmd = Parse Sort String | Infer Sort String | Quit | Help | None | TypeCheck String String

repl :: IO ()
repl = runInputT defaultSettings $ repl' Map.empty
    where
        getCmd ":q"                  = Quit

        getCmd (':' : 't' : ' ' : ty) = Infer Type ty
        getCmd (':' : 'k' : ' ' : k) = Infer Kind k

        getCmd (':' : 'p' : 'e' : ' ' : e) = Parse Term e
        getCmd (':' : 'p' : 't' : ' ' : ty) = Parse Type ty
        getCmd (':' : 'p' : 'k' : ' ' : k) = Parse Kind k

        getCmd input | [":check", expr, ty] <- words input = TypeCheck expr ty

        getCmd ":h"                  = Help
        getCmd _                     = None

        repl' env = let
            evalExpr line = do
                expr <- runSagaExpr line
                runStateT (E.eval expr) env
            evalDec line = do
                dec <- runSagaDec line
                runStateT (E.evalDeclaration dec) env

            parseExpr input = case evalExpr input <> evalDec input of
                (Left e)          -> do
                    pPrint e
                    repl' env
                (Right (v, env')) -> do
                    outputStrLn "Value:"
                    pPrint v
                    outputStrLn "\nEnv:"
                    pPrint env'
                    repl' env'

            typecheck expr ty = let
                    parsed = do
                        HMPI.Parsed expr' _ _ <- HMP.runSagaExpr expr
                        HMPI.Parsed tyExpr' _ _ <- HMP.runSagaType ty
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
                case HMP.runSagaExpr input of
                    (Left e)  -> pPrint e
                    (Right t) -> pPrint t
                repl' env

            parseType input = do
                case HMP.runSagaType input of
                    (Left e)   -> pPrint e
                    (Right ty) -> pPrint ty
                repl' env

            parseKind input = do
                case HMP.runSagaKind input of
                    (Left e)   -> pPrint e
                    (Right ty) -> pPrint ty
                repl' env

            inferType input = do
                case HMI.run input of
                    (Left e)   -> pPrint e
                    (Right ty) -> pPrint ty
                repl' env
            inferKind input = do
                case Infer.inferKind input of
                    (Left e)  -> pPrint e
                    (Right k) -> pPrint k
                repl' env

            in do
                (Just line) <- getInputLine "Saga λ> "
                case getCmd line of
                    Quit -> return ()
                    None -> parseExpr line
                    Infer Type ty -> inferType ty
                    Infer Kind k -> inferKind k
                    Parse Term e -> parseTerm e
                    Parse Type ty -> parseType ty
                    Parse Kind k -> parseKind k
                    TypeCheck expr ty -> do
                        typecheck expr ty
                        repl' env
                    _    -> do
                        outputStrLn "Not implemented yet!"
                        repl' env




script :: FilePath -> IO ()
script fp =
    let
        check' (Scripts.Let (AST.Name _ id) tyExpr kind expr) = case (tyExpr, kind) of
            (Just tyExpr', Just kind') -> do
                check_kind tyExpr' kind'
                inferred <- Infer.typeof expr
                ty <- Infer.reduce tyExpr'
                matches <- check expr tyExpr'
                update id inferred ty matches

            (Just tyExpr', Nothing) -> do
                k <- Infer.kindOf tyExpr'
                check_kind tyExpr' k
                inferred <- Infer.typeof expr
                ty <- Infer.reduce tyExpr'
                matches <- check expr tyExpr'
                update id inferred ty matches

            (Nothing, Just kind') -> do
                inferred <- Infer.typeof expr
                let tyExpr' = T.Type inferred
                check_kind tyExpr' kind'
                matches <- check expr tyExpr'
                update id inferred inferred matches

            (Nothing, Nothing) -> do
                inferred <- Infer.typeof expr
                let tyExpr' = T.Type inferred
                k <- Infer.kindOf tyExpr'
                check_kind tyExpr' k
                matches <- check expr tyExpr'
                update id inferred inferred matches

        check' (Scripts.Type (AST.Name _ id) kind tyExpr) = do
            k <- Infer.kindOf tyExpr
            ty <- Infer.reduce tyExpr
            matches <- check_kind tyExpr k
            if not matches
                then throwError $ Infer.WrongKind ty k
                else do
                    modify $ \s -> s{ Infer.typeVars = Map.insert id ty $ Infer.typeVars s }
                    return matches



        update :: String -> T.Type a -> T.Type a -> Bool -> Infer.Infer a Bool
        update id inferred ty matches = if not matches
            then throwError $ Infer.TypeMismatch inferred ty
            else do
                modify $ \s -> s{ Infer.expressions = Map.insert id ty $ Infer.expressions s }
                return matches

        typecheck' str = do
            (Scripts.Script _ _ decs _) <- runSagaScript str
            results <- Infer.doInEnv Nothing $ mapM check' decs
            return $  and `first` results
        eval' str = do
            (Scripts.Script _ _ decs _) <- runSagaScript str
            runStateT (mapM E.evalDeclaration decs) Map.empty


    in do
        handle <- openFile fp ReadMode
        evalH <- openFile "./lang/evaluation.log" WriteMode
        tcH <- openFile "./lang/typecheck.log" WriteMode

        contents <- hGetContents handle
        case typecheck' contents of
            Left e -> putStrLn e
            Right val -> do
                case val of
                    (False, env) -> do
                        putStrLn "Found errors!"
                        pPrint env
                        pHPrint tcH env
                    (True, env) -> do
                        putStrLn "No errors found!"
                        pPrint env
                        pHPrint tcH env
                        case eval' contents of
                            Left e           -> putStrLn e
                            Right (val, env) -> do
                                -- putStrLn "Value:"
                                -- pPrint val
                                -- putStrLn "\n\nEnv:"
                                -- pPrint env
                                pHPrint evalH val
                                pHPrint evalH "\n\n------------------------------------\n------------------------------------\n\nEnv:\n"
                                pHPrint evalH env


        hClose handle
        hClose evalH
        hClose tcH
        putStrLn "Bye!"


parseScript :: FilePath -> IO ()
parseScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle

    pPrint $ HMP.runSagaScript contents
    putStrLn "Bye!"


lexScript :: FilePath -> IO ()
lexScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    pPrint (L.scanMany contents)
    hClose handle
    putStrLn "Bye!"


lex :: String -> IO ()
lex input = do
    pPrint (L.scanMany input)
    putStrLn "Bye!"


