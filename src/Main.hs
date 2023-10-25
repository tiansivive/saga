{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           REPL.Repl                            (repl)

import qualified Saga.Lexer.Lexer                     as L
import qualified Saga.Parser.Parser                   as P

import qualified Saga.Language.TypeSystem.Elaboration as Elab

import           Control.Monad.Except                 (runExcept)
import           Data.Bifunctor                       (first)
import           Debug.Trace                          (traceM)
import           Saga.Language.Core.Expr              (Script (Script))
import           Saga.Language.Generation.JS          (Generator (generate))

import           Saga.Language.TypeSystem.Elaboration (elaborateScript)
import           Saga.Language.TypeSystem.Inference   (inferScript)
import           Saga.Language.TypeSystem.Lib         (defaultEnv)
import           Saga.Parser.Desugar                  (desugarExpr,
                                                       desugarScript)
import           Saga.Parser.ParsingInfo              (script)
import           Saga.Parser.Shared                   (ParsedData (Parsed))
import           System.Console.Haskeline             (defaultSettings,
                                                       getInputLine,
                                                       outputStrLn, runInputT)
import           System.IO                            (IOMode (ReadMode, ReadWriteMode, WriteMode),
                                                       hClose, hGetContents,
                                                       hPrint, hPutStr,
                                                       openFile)
import           Text.Pretty.Simple                   (pHPrint, pPrint)



main :: IO ()
main = do
    putStrLn "Starting Saga..."
    compileFile "lang/v2.saga" "lang/v2.js"

parseFile :: FilePath -> IO ()
parseFile fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    let res = fmap desugarScript <$> P.runSagaScript contents
    pPrint res
    pHPrint parsingH res
    hClose handle
    hClose parsingH
    putStrLn "Bye!"

inferFile :: FilePath -> IO ()
inferFile fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    case run' contents of
        Left err -> pPrint err
        Right (decs, state, trace) -> do
            pPrint decs
            pHPrint parsingH trace
            pHPrint parsingH state
            pHPrint parsingH decs

    hClose handle
    hClose parsingH
    putStrLn "Bye!"

    where
        run' contents = do
            (Parsed script _ _) <- fmap desugarScript <$> P.runSagaScript contents
            show `first` inferScript defaultEnv script


compileFile :: FilePath -> FilePath -> IO ()
compileFile fp outFp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    outputH <- openFile outFp WriteMode
    contents <- hGetContents handle
    --pPrint $ fmap desugarExpr <$> P.runSagaExpr contents

    let output = do
            (Parsed script _ _) <- fmap desugarScript <$> P.runSagaScript contents
            (decs, st, acc) <- show `first` inferScript defaultEnv script
            traceM $ show st
            (script, st', acc') <- show `first` elaborateScript st (Script decs)
            traceM $ show st'
            return (generate script, st', acc <> acc')

    case output of
        Left err -> do
            pPrint err
            pHPrint parsingH err
        Right (code, st, acc) -> do
            --pPrint (st, acc)
            pHPrint parsingH (st, acc)
            hPutStr outputH code

    hClose handle
    hClose parsingH
    hClose outputH
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


