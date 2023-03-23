module Main (main) where

import Saga.Parser.Parser (runSagaScript, runSagaExpr)
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
    putStrLn $ show $ runSagaExpr line
    eval

script :: FilePath -> IO ()
script fp = do
    handle <- openFile fp ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ runSagaScript contents
    hClose handle
    putStrLn "Bye!"