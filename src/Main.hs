{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           REPL.Repl                                        (repl)

import qualified Saga.Lexer.Lexer                                 as L
import qualified Saga.Parser.Parser                               as P


import           Saga.Language.Generation.JS                      (Generator (generate))
import           Saga.Language.TypeSystem.HindleyMilner.Check     (checkScript)
import           Saga.Language.TypeSystem.HindleyMilner.Inference (run)
import           Saga.Parser.Desugar                              (desugarExpr,
                                                                   desugarScript)
import           Saga.Parser.ParsingInfo                          (script)
import           Saga.Parser.Shared                               (ParsedData (Parsed))
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
    typecheckScript "lang/v2.saga"


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
    --pPrint $ fmap desugarExpr <$> P.runSagaExpr contents
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



typecheckScript :: FilePath -> IO ()
typecheckScript fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    let res = fmap desugarScript <$> P.runSagaScript contents
    case res of
        Left parseErr -> pPrint parseErr
        Right (Parsed script _ _) -> do
            let (results, state, acc) = checkScript script
            let extract (r, _, _) = r

            mapM_ (pPrint . fmap extract) results
            pHPrint parsingH "\n---------------------------\nRESULTS\n---------------------------\n"
            pHPrint parsingH results
            pHPrint parsingH "\n----------------------------\nSTATE\n----------------------------\n"
            pHPrint parsingH state
            pHPrint parsingH "\n-------------------------\nACCUMULATED\n-------------------------\n"
            pHPrint parsingH acc


    hClose handle
    hClose parsingH
    putStrLn "Bye!"


genScript :: FilePath -> IO ()
genScript fp = do
    handle <- openFile fp ReadMode
    parsingH <- openFile "./lang/test.parsing.log" WriteMode
    contents <- hGetContents handle
    let res = fmap desugarScript <$> P.runSagaScript contents
    let output = case res of
            Left err                  -> err
            Right (Parsed script _ _) -> generate script
    pPrint output
    pHPrint parsingH output
    hClose handle
    hClose parsingH
    putStrLn "Bye!"


lex :: String -> IO ()
lex input = do
    pPrint (L.scanMany input)
    putStrLn "Bye!"


