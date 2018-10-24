module FirstPrinciples where

import Data.Char
import Data.Time


-- CYPHER

encode :: Char -> Int
encode ch = ord ch - ord 'a'

decode :: Int -> Char
decode n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = decode $ mod (f (encode ch) n) 26

caesar :: Int -> String -> String
caesar n = map (shift (+) n)

decaesar :: Int -> String -> String
decaesar n = map (shift (-) n)



-- DATABASE

data DatabaseItem
  = DBString String
  | DBNumber Integer
  | DBDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DBDate (UTCTime (fromGregorian 1995 5 1) (secondsToDiffTime 123))
  , DBNumber 9001
  , DBString "Hello world!"
  , DBDate (UTCTime (fromGregorian 1997 5 1) (secondsToDiffTime 123))
  , DBNumber 66
  , DBString "test"
  ]

filterDBDate :: [DatabaseItem] -> [UTCTime]
filterDBDate db = [t | (DBDate t) <- db]

filterDBNumber :: [DatabaseItem] -> [Integer]
filterDBNumber db = [n | (DBNumber n) <- db]

avgDB :: [DatabaseItem] -> Double
avgDB db =
    let nums = filterDBNumber db in
        if length nums == 0
            then 0
            else (fromIntegral (sum nums)) / (fromIntegral (length nums))


