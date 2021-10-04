module QuoteData
    ( parseCSV
    , QuoteData(..)
    ) where

import           Control.Monad                  ( guard
                                                , replicateM
                                                )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BC
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Time                      ( Day
                                                , fromGregorianValid
                                                )
import           Text.Parsec                    ( ParseError
                                                , char
                                                , choice
                                                , digit
                                                , many1
                                                , parse
                                                , try
                                                )
import           Text.Parsec.ByteString.Lazy    ( Parser )

data QuoteData = QuoteData
    { day    :: Day
    , volume :: Int
    , open   :: Double
    , close  :: Double
    , high   :: Double
    , low    :: Double
    }
    deriving Show

pDate :: Parser (Maybe Day)
pDate = do
    year <- replicateM 4 digit
    char '-'
    month <- choice $ (`replicateM` digit) <$> [2, 1]
    char '-'
    day <- choice $ try . (`replicateM` digit) <$> [2, 1]
    return $ fromGregorianValid (read year) (read month) (read day)

pNumber :: (Num a, Read a) => Parser a
pNumber = read <$> choice
    (   try
    <$> [ do
            d1 <- many1 digit
            char '.'
            d2 <- many1 digit
            return $ d1 ++ "." ++ d2
        , many1 digit
        ]
    )

pRow :: Char -> Parser QuoteData
pRow sep = do
    d <- pDate
    guard $ isJust d
    char sep
    c <- pNumber
    char sep
    v <- pNumber
    char sep
    o <- pNumber
    char sep
    h <- pNumber
    char sep
    l <- pNumber
    return $ QuoteData { day = fromJust d, volume = v, open = o, close = c, high = h, low = l }

parseRow :: Char -> BL.ByteString -> Either ParseError QuoteData
parseRow sep = parse (pRow sep) ""

parseRows :: Char -> [BL.ByteString] -> [QuoteData]
parseRows _   []       = []
parseRows sep (x : xs) = case parseRow sep x of
    Right v -> v : parseRows sep xs
    Left  _ -> parseRows sep xs

parseCSV :: BL.ByteString -> [QuoteData]
parseCSV table =
    let rows = tail $ BC.lines table
        sep  = ','
    in  parseRows sep rows
