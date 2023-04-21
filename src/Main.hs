{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Saga.AST.Syntax          as AST
import qualified Saga.AST.Scripts         as Scripts
import qualified Saga.Lexer.Lexer         as L
import           Saga.Parser.Parser       (runSagaDec, runSagaExpr,
                                           runSagaScript, runSagaType)

import qualified Data.Map                 as Map
import qualified Saga.AST.Evaluation      as E
import qualified Saga.AST.TypeSystem.Inference       as Infer

import           Control.Monad.State.Lazy
import           Data.Maybe               (fromJust)

import           Saga.AST.TypeSystem.Check           (check)
import           System.Console.Haskeline
import           System.IO                (IOMode (ReadMode, ReadWriteMode, WriteMode),
                                           hClose, hGetContents, openFile)
import           Text.Pretty.Simple       (pPrint)






main :: IO ()
main = do
    putStrLn "Starting Saga..."
    line <- getLine
    pPrint (runSagaExpr line)


data SagaCmd = Type String | Quit | Help | None | TypeCheck String String

repl :: IO ()
repl = runInputT defaultSettings $ repl' Map.empty
    where
        getCmd ":q"                  = Quit
        getCmd (':' : 't' :' ' : ty) = Type ty
        getCmd ":h"                  = Help
        getCmd input | [":check", expr, ty] <- words input = TypeCheck expr ty
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
                        expr' <- runSagaExpr expr
                        ty' <- runSagaType ty
                        inferred <- Infer.infer expr
                        return (expr', ty', inferred)
                in case parsed of
                    Left e -> pPrint e
                    Right (expr', ty', inferred) -> do
                        outputStrLn "Expression:"
                        pPrint expr'
                        outputStrLn "\nInferred Type:"
                        pPrint inferred
                        outputStrLn "\nType:"
                        pPrint ty'
                        outputStrLn "\nTypecheck:"
                        pPrint $ Infer.run $ check expr' ty'

            parseType input = do
                case runSagaType input of
                    (Left e)   -> pPrint e
                    (Right ty) -> pPrint ty
                repl' env

            in do
                (Just line) <- getInputLine "Saga λ> "
                case getCmd line of
                    Quit -> return ()
                    None -> parseExpr line
                    Type ty -> parseType ty
                    TypeCheck expr ty -> do
                        typecheck expr ty
                        repl' env
                    _    -> do
                        outputStrLn "Not implemented yet!"
                        repl' env




script :: FilePath -> IO ()
script fp =
    let
        script' str = do
            (Scripts.Script _ _ decs _) <- runSagaScript str
            runStateT (mapM E.evalDeclaration decs) Map.empty


    in do
        handle <- openFile fp ReadMode
        contents <- hGetContents handle
        case script' contents of
            Left e           -> putStrLn e
            Right (val, env) -> do
                putStrLn "Value:"
                pPrint val
                putStrLn "\nEnv:"
                pPrint env
        hClose handle
        putStrLn "Bye!"


parseScript :: FilePath -> IO (Either String (Scripts.Script L.Range))
parseScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    return $ runSagaScript contents


lexScript :: FilePath -> IO ()
lexScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    pPrint (L.scanMany contents)
    hClose handle
    putStrLn "Bye!"
