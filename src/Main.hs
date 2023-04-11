{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Saga.AST.Syntax          as AST
import qualified Saga.Lexer.Lexer         as L
import           Saga.Parser.Parser       (runSagaDec, runSagaExpr,
                                           runSagaScript, runSagaType)

import qualified Data.Map                 as Map
import qualified Saga.AST.Evaluation      as E
import qualified Saga.AST.Inference       as Infer

import           Control.Monad.State.Lazy
import           Data.Maybe               (fromJust)
import           System.Console.Haskeline
import           System.IO                (IOMode (ReadMode), hClose,
                                           hGetContents, openFile)



main :: IO ()
main = do
    putStrLn "Starting Saga..."
    line <- getLine
    print (runSagaExpr line)


data SagaCmd = Type String | Quit | Help | None

repl :: IO ()
repl = runInputT defaultSettings $ repl' Map.empty
    where

        getCmd ":q"             = Quit
        getCmd (':':'t':' ':ty) = Type ty
        getCmd ":h"             = Help
        getCmd _                = None

        repl' env = let
            evalExpr line = do
                expr <- runSagaExpr line
                runStateT (E.eval expr) env
            evalDec line = do
                dec <- runSagaDec line
                runStateT (E.evalDeclaration dec) env

            parseExpr input = case evalExpr input <> evalDec input of
                (Left e)          -> do
                    outputStrLn e
                    repl' env
                (Right (v, env')) -> do
                    outputStrLn $ show v <> "\n" <> show env
                    repl' env'

            parseType input = do
                outputStrLn $ case runSagaType input of
                    (Left e)   -> e
                    (Right ty) -> show ty
                repl' env

            in do
                (Just line) <- getInputLine "Saga λ> "
                case getCmd line of
                    Quit -> return ()
                    None -> parseExpr line
                    Type ty -> parseType ty
                    _    -> do
                        outputStrLn "Not implemented yet!"
                        repl' env



script :: FilePath -> IO ()
script fp =
    let
        script' str = do
            (AST.Script _ _ decs _) <- runSagaScript str
            runStateT (mapM E.evalDeclaration decs) Map.empty


    in do
        handle <- openFile fp ReadMode
        contents <- hGetContents handle
        case script' contents of
            Left e           -> putStrLn e
            Right (val, env) -> do
                putStrLn $ "Value: " <> show val
                putStrLn $ "Env: " <> show env
        hClose handle
        putStrLn "Bye!"


parseScript :: FilePath -> IO ()
parseScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    print (runSagaScript contents)
    hClose handle
    putStrLn "Bye!"

lexScript :: FilePath -> IO ()
lexScript fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    print (L.scanMany contents)
    hClose handle
    putStrLn "Bye!"
