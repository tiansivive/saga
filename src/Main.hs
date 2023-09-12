{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           REPL.Repl                                        (repl)

import qualified Saga.Lexer.Lexer                                 as L
import qualified Saga.Parser.Parser                               as P

import           Saga.Language.TypeSystem.HindleyMilner.Inference (run)
import           Saga.Parser.Desugar                              (desugarExpr,
                                                                   desugarScript)
import           System.Console.Haskeline                         (defaultSettings,
                                                                   getInputLine,
                                                                   outputStrLn,
                                                                   runInputT)
import           System.IO                                        (IOMode (ReadMode, ReadWriteMode, WriteMode),
                                                                   hClose,
                                                                   hGetContents,
                                                                   openFile)
import           Text.Pretty.Simple                               (pHPrint,
                                                                   pPrint)






main :: IO ()
main = do
    putStrLn "Starting Saga..."
    repl







parseScript :: FilePath -> IO ()
parseScript fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    let res = fmap desugarScript <$> P.runSagaScript contents
    pPrint res
    pHPrint parsingH res
    hClose handle
    hClose parsingH
    putStrLn "Bye!"

inferScript :: FilePath -> IO ()
inferScript fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    pPrint $ fmap desugarExpr <$> P.runSagaExpr contents
    let res = run contents
    pPrint res
    pHPrint parsingH res
    hClose handle
    hClose parsingH
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


