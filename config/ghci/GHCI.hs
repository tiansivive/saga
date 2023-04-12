

module Config.GHCI where

import Text.Pretty.Simple (pHPrint)
import           System.IO   


-- logFile = "./logs/ghci.log"
-- maxLogLength = 1024 -- max length of a single write

-- plog x = appendFile logFile (show x ++ "\n") >> pPrint x


pplog saga log f = do
    hSaga <- openFile saga ReadMode
    hLog <- openFile log WriteMode
    contents <- hGetContents hSaga
    let result = f contents
    pHPrint hLog result
    hClose hSaga
    hClose hLog
    return result