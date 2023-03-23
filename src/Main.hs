module Main (main) where

import Saga.Parser.Parser (runSagaScript, runSagaExpr)

import qualified Saga.AST.Evaluation as E
import System.IO

main :: IO ()
main = do
    putStrLn "Starting Saga..."
    line <- getLine
    putStrLn $ show $ runSagaExpr line



eval :: IO ()
eval = do
    putStr "Saga> "
    line <- getLine
    putStrLn $ case runSagaExpr line of
        (Right expr) -> show $ E.eval expr
        (Left err) -> show err
    eval

script :: FilePath -> IO ()
script fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ runSagaScript contents
    hClose handle
    putStrLn "Bye!"