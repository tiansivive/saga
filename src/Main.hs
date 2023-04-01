{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Saga.AST.Syntax          as AST
import qualified Saga.Lexer.Lexer         as L
import           Saga.Parser.Parser       (runSagaExpr, runSagaScript)

import qualified Data.Map                 as Map
import qualified Saga.AST.Evaluation      as E

import           Control.Monad.State.Lazy
import           System.IO

main :: IO ()
main = do
    putStrLn "Starting Saga..."
    line <- getLine
    print (runSagaExpr line)



repl :: IO ()
repl =
    let
        value line = do
            expr <- runSagaExpr line
            E.runEvaluation expr

    in do
        putStr "Saga λ> "
        line <- getLine
        putStrLn $ case value line of
            (Right (v, env)) -> show v <> "\n" <> show env
            (Left e)         -> e
        repl



script :: FilePath -> IO ()
script fp =
    let
        eval' (AST.Define _ name expr _) = E.eval (AST.Assign name expr)
        script' str = do
            (AST.Script _ _ decs _) <- runSagaScript str
            runStateT (mapM eval' decs) Map.empty

    in do
        handle <- openFile fp ReadMode
        contents <- hGetContents handle
        print (script' contents)
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
