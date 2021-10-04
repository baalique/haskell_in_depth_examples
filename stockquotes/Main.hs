module Main where

import           Control.Monad                  ( guard )
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                    ( fromRight
                                                , isRight
                                                )
import           Params
import           QuoteData
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
    print $ parseCSV csvData
