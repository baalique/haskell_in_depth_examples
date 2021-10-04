module Main where

import           Params                         ( getParams )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ case getParams args of
        Left  err -> err
        Right res -> show res
