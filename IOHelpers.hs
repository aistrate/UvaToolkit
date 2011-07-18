module IOHelpers (processFile, printTime) where

import System.IO
import System.Time
import Text.Printf


processFile :: String -> String -> (String -> String) -> IO ()
processFile inFile outFile procLine =
  do hIn <- openFile inFile ReadMode
     cIn <- hGetContents hIn
     let ls = filter (not . null) $ lines cIn
     let cOut = unlines $ map procLine ls
     hOut <- openFile outFile WriteMode
     hPutStr hOut cOut


printTime :: IO a -> IO a
printTime f = do t1 <- getClockTime
                 r <- f
                 t2 <- getClockTime
                 let td = diffClockTimes t2 t1
                     sec = fromIntegral (tdSec td) +
                           fromIntegral (tdPicosec td) / 1e12 :: Double
                 printf "Time: %.6f sec.\n" sec
                 return r
