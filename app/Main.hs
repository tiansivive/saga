module Main where

import           Saga.Parser.Parser (runSaga)

main :: IO ()
main = do
    putStrLn "Starting Saga..."
    line <- getLine

