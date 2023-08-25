{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Saga.Lexer.Lexer                              as L


import qualified Data.Map                                      as Map
import qualified Saga.AST.Evaluation                           as E
import qualified Saga.AST.TypeSystem.HindleyMilner.Check       as HMC
import qualified Saga.AST.TypeSystem.HindleyMilner.Environment as HME
import qualified Saga.AST.TypeSystem.HindleyMilner.Inference   as HMI
import qualified Saga.AST.TypeSystem.HindleyMilner.Refinement  as HMR
import           Saga.AST.TypeSystem.HindleyMilner.Types       as HMT hiding
                                                                      (Term,
                                                                       Type)
import qualified Saga.Parser.ParserHM                          as HMP
import qualified Saga.Parser.ParsingInfo                       as HMPI

import           Control.Monad.State.Lazy
import           Data.Maybe                                    (fromJust)

import           Control.Monad.Except
import           Control.Monad.RWS                             (evalRWST)
import           Data.Bifunctor                                (first)
import           Debug.Trace                                   (traceM)

import qualified Saga.AST.TypeSystem.HindleyMilner.Refinement  as HMR
import           Saga.AST.TypeSystem.HindleyMilner.Refinement  (refine)

import           REPL.Repl                                     (repl)
import           Saga.Parser.ParsingInfo                       (ParsedData (Parsed))
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
    repl



parseScript :: FilePath -> IO ()
parseScript fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    let res = HMP.runSagaScript contents
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


