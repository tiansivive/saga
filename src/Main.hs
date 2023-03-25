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
        putStr "Saga> "
        line <- getLine
        putStrLn $ case value line of
            (Right (v, env)) -> show v <> "\n" <> show env
            (Left e)         -> e
        repl





script :: FilePath -> IO ()
script fp =
    let
        eval' def = E.eval (AST.Declaration def)
        script' str = do
            (AST.Script _ _ defs _) <- runSagaScript str
            runStateT (mapM eval' defs) Map.empty

    in do
        handle <- openFile fp ReadMode
        contents <- hGetContents handle
        print (script' contents)
        hClose handle
        putStrLn "Bye!"

