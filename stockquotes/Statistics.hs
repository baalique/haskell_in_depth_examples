module Statistics where

import           Data.List
import           Fmt
import           QuoteData

data Field = Open | Close | High | Low | Volume deriving Show

data StatEntry = StatEntry
    { field             :: Field
    , meanVal           :: StatValue
    , minVal            :: StatValue
    , maxVal            :: StatValue
    , daysBetweenMinMax :: Int
    }
    deriving Show

data StatValue = StatValue
    { decimalPlaces :: Int
    , value         :: Double
    }
    deriving Show

instance Buildable StatValue where
  build v = fixedF (decimalPlaces v) (value v)

data Stats = Stats
    { openStats   :: StatEntry
    , closeStats  :: StatEntry
    , lowStats    :: StatEntry
    , highStats   :: StatEntry
    , volumeStats :: StatEntry
    }
    deriving Show

getStats :: [QuoteData] -> Int -> Stats
getStats qd prec = Stats o c l h v
  where
    o = getStatParam open Open prec qd
    c = getStatParam close Close prec qd
    l = getStatParam low Low prec qd
    h = getStatParam high High prec qd
    v = getStatParam (fromIntegral . volume) Volume prec qd

getStatParam :: (QuoteData -> Double) -> Field -> Int -> [QuoteData] -> StatEntry
getStatParam f fd prec qd = StatEntry fd meanV minV maxV daysBetween
  where
    meanV       = StatValue prec (sum (map f qd) / fromIntegral (length qd))
    minV        = StatValue prec (minimum $ map f qd)
    maxV        = StatValue prec (maximum $ map f qd)
    daysBetween = getDaysBetween (map f qd)

getDaysBetween :: [Double] -> Int
getDaysBetween ds = abs (minDay - maxDay)
  where
    minDay  = head indices
    maxDay  = last indices
    indices = map fst $ sortOn snd $ zip [0 ..] ds

decimalPrecision :: Int
decimalPrecision = 2

showPrice :: Double -> Builder
showPrice = fixedF decimalPrecision

getStatsEntriesFromStats :: Stats -> [StatEntry]
getStatsEntriesFromStats stats =
  [openStats stats, closeStats stats, lowStats stats, highStats stats, volumeStats stats]
