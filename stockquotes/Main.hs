module Main where

import           Control.Monad                  ( guard
                                                , when
                                                )
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                    ( fromRight
                                                , isRight
                                                )
import           Data.Maybe                     ( isJust )
import           HtmlReport
import           Params
import           QuoteData
import           Statistics
import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ case getParams args of
        Left  err -> err
        Right res -> show res
    let params = getParams args
    guard $ isRight params
    csvData <- BL.readFile (filename $ fromRight undefined params)
    let parsed = parseCSV csvData
        stats  = getStats parsed 4
        html   = htmlFilename $ fromRight undefined params
    saveHtml html (htmlReport "Stockquotes data" parsed (getStatsEntriesFromStats stats))
    print stats
