module Params
    ( getParams, Params(..),
    ) where

import           System.Console.GetOpt          ( ArgDescr(NoArg, ReqArg)
                                                , ArgOrder(Permute)
                                                , OptDescr(..)
                                                , getOpt
                                                )

data Params = Params
    { filename     :: FilePath
    , companyName  :: Maybe String
    , hasChart     :: Bool
    , htmlFilename :: Maybe FilePath
    , isSilent     :: Bool
    }
    deriving Show

defaultParams :: Params
defaultParams =
    Params { filename = "", companyName = Nothing, hasChart = False, htmlFilename = Nothing, isSilent = False }

usage :: String
usage =
    "Usage: stockquotes FILE [-n|--name ARG] [-c|--chart] [--html FILE] [-s|--silent]\n\
    \  Stock quotes data processing\n\
    \\n\
    \Available options:\n\
    \  FILE                CSV file name\n\
    \  -n,--name ARG       Company name\n\
    \  -c,--chart          Generate chart\n\
    \  --html FILE         Generate HTML report\n\
    \  -s,--silent         Don't print statistics\n\
    \  -h,--help           Show this help text"

options :: [OptDescr (Params -> Either String Params)]
options =
    [ Option ['n'] ["name"] (ReqArg (\d opts -> return $ opts { companyName = Just d }) "FILE") "Company name"
    , Option ['c'] ["chart"]  (NoArg (\opts -> return $ opts { hasChart = True })) "Generate chart"
    , Option [] ["html"] (ReqArg (\d opts -> return $ opts { htmlFilename = Just d }) "FILE") "Generate HTML report"
    , Option ['s'] ["silent"] (NoArg (\opts -> return $ opts { isSilent = True })) "Don't print statistics"
    , Option ['h'] ["help"]   (NoArg (\_ -> Left usage))                           "Show this help text"
    ]

getParams :: [String] -> Either String Params
getParams args = case getOpt Permute options args of
    (_, [], _   ) -> Left "You should provide a valid filename"
    (o, n , []  ) -> foldl (>>=) (Right $ defaultParams { filename = head n }) o
    (_, _ , errs) -> Left $ concat errs
