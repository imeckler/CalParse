-----------------------------------------------------------------------------
-- |
-- Module      :  ParseDateTime
-- Copyright   :  (c) Izaak Meckler 2012
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  izaakmeckler@me.com
-- Stability   :  provisional
-- Portability :  FlexibleContexts
-- 
-- Parse English language dates and times
-- 
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts, TupleSections #-}

module ParseDateTime
    ( parseTimeOfDay
    , parseTimeOfDayWord
    , parseTimeOfDayNum
    , hoursWord
    , minutesWord
    , dayOfWeekWord
    , weekdayAndTime
    , monthWord
    , slashDate
    ) where

import ParseCommon
import Data.Time.LocalTime
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Time.Calendar
import Debug.Trace
import Data.List (minimumBy, foldl')
import Data.Function (on)
import Text.EditDistance
import Data.Maybe (fromJust)
import Data.Char (digitToInt, toLower)
import Control.Arrow (first)

type Map a b = [(a, b)]

digitWordToInt :: [(String, Int)]
digitWordToInt = [ ("zero",  0)
                 , ("one"  , 1)
                 , ("two"  , 2)
                 , ("three", 3)
                 , ("four" , 4)
                 , ("five" , 5)
                 , ("six"  , 6)
                 , ("seven", 7)
                 , ("eight", 8)
                 , ("nine" , 9)
                 ]

teenWordToInt :: [(String, Int)]
teenWordToInt = [("ten"      , 10)
                ,("eleven"   , 11)
                ,("twelve"   , 12)
                ,("thirteen" , 13)
                ,("fourteen" , 14)
                ,("fifteen"  , 15)
                ,("sixteen"  , 16)
                ,("seventeen", 17)
                ,("eighteen" , 18)
                ,("nineteen" , 19)
                ]

tensWordToInt :: [(String, Int)]
tensWordToInt = [ ("ten"    , 10)
                , ("twenty" , 20)
                , ("thirty" , 30)
                , ("forty"  , 40)
                , ("fifty"  , 50)
                , ("sixty"  , 60)
                , ("seventy", 70)
                , ("eighty" , 80)
                , ("ninety" , 90)
                ]

dayWordToInt :: [(String, Int)]
dayWordToInt = [("monday"   , 1)
               ,("tuesday"  , 2)
               ,("wednesday", 3)
               ,("thursday" , 4)
               ,("friday"   , 5)
               ,("saturday" , 6)
               ,("sunday"   , 7)
               ]

monthFullToInt :: [(String, Int)]
monthFullToInt = [("january"  , 1)
                 ,("february" , 2)
                 ,("march"    , 3)
                 ,("april"    , 4)
                 ,("may"      , 5)
                 ,("june"     , 6)
                 ,("july"     , 7)
                 ,("august"   , 8)
                 ,("september", 9)
                 ,("october"  , 10)
                 ,("november" , 11)
                 ,("december" , 12)
                 ]

monthWordToInt :: [(String, Int)]
monthWordToInt = monthFullToInt ++ map (first $ take 3) monthFullToInt

monthWords :: [String]
monthWords = map fst monthWordToInt

dayWords :: [String]
dayWords = map fst dayWordToInt

lookupWord :: Stream s m Char => String -> Map String b -> ParsecT s u m b
lookupWord s mapping = word >>=
    maybe (fail s) return . 
    flip lookup mapping

-- | @parseTimeOfDay@ parses a time of day either as \"H*:M*\" or English words, e.g
-- \"two fifty four\"
parseTimeOfDay :: Stream s m Char => ParsecT s u m TimeOfDay
parseTimeOfDay = try parseTimeOfDayWord <|> parseTimeOfDayNum

-- | @parseTimeOfDayWord@ parses an English language time of day description
parseTimeOfDayWord :: Stream s m Char => ParsecT s u m TimeOfDay
parseTimeOfDayWord =
    TimeOfDay <$> (hours <* spaces) 
              <*> option 0 minutes
              <*> return 0

-- | @parseTimeOfDayNum@ parses a time of day as \"H*:M*\"
parseTimeOfDayNum :: Stream s m Char => ParsecT s u m TimeOfDay
parseTimeOfDayNum =
    TimeOfDay <$> positiveNatural
              <*> (optional (char ':') *> option 0 positiveNatural)
              <*> return 0

positiveNatural :: Stream s m Char => ParsecT s u m Int
positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

digitRange :: Stream s m Char => Int -> Int -> ParsecT s u m Int
digitRange a b = lookupWord "failed to recognize digit" . drop a . take (b + 1) $ tensWordToInt

-- | @digitWord@ matches one of \"one\", \"two\", \"three\", \"four\", \"five\",
-- | \"six\", \"seven\", \"eight\", \"nine\" to the corresponding @Int@

digitWord :: Stream s m Char => ParsecT s u m Int
digitWord = lookupWord "failed to recognize digit word" digitWordToInt

traceStr x = trace x x

parseNatInRange :: Stream s m Char => Int -> Int -> ParsecT s u m Int
parseNatInRange a b = do
    n <- positiveNatural
    if a <= n && n <= b then return n else fail "number out of range"

-- | @hours@ matches a word representing a number or a number between 0 and 23
hours :: Stream s m Char => ParsecT s u m Int
hours = hoursWord <|> parseNatInRange 0 23

-- | @hours@ matches a word representing a number or a number between 0 and 59
minutes :: Stream s m Char => ParsecT s u m Int
minutes = minutesWord <|> parseNatInRange 0 59

-- | @hoursWord@ matches a word representing a number between 0 and 23

hoursWord :: Stream s m Char => ParsecT s u m Int
hoursWord = try digitWord <|> try teenWord <|> rest
    where rest = (+ 20) <$> (string "twenty" >> spaces >> option 0 (try $ digitRange 1 3))

-- | @minutesWord@ matches a word representing a number between 1 and 59

minutesWord :: Stream s m Char => ParsecT s u m Int
minutesWord = (try $ string "oh" >> spaces >> digitWord) 
           <|> try ((+) <$> tensRange 1 5 <* spaces
                        <*> option 0 (try digitWord))
           <|> teenWord 

teenWord :: Stream s m Char => ParsecT s u m Int
teenWord = lookupWord "failed to recognize -teen word" teenWordToInt

tensRange :: Stream s m Char => Int -> Int -> ParsecT s u m Int
tensRange a b = lookupWord "failed to recognize number word" . drop (a - 1) . take b $ tensWordToInt

tensWord :: Stream s m Char => ParsecT s u m Int
tensWord = lookupWord "failed to recognize number word" tensWordToInt

minDistance :: Stream s m Char => Int -> [String] -> ParsecT s u m String
minDistance min ws = word >>= \w ->
    let (dist, w') = minimumBy (compare `on` fst) .
                     map (\x -> (editDistance (map toLower w) x, x)) $ ws
    in if dist > min
       then fail "failed to match word closely"
       else return w'

dayOfWeekWord :: Stream s m Char => ParsecT s u m Int
dayOfWeekWord = fromJust . flip lookup dayWordToInt <$> minDistance 4 dayWords

editDistance :: String -> String -> Int
editDistance = levenshteinDistance defaultEditCosts

weekdayAndTime :: Stream s m Char => ParsecT s u m (Int, TimeOfDay)
weekdayAndTime = try dayFirst <|> timeFirst
    where dayFirst = (,) <$> dayOfWeekWord <* spaces 
                                           <* optional (try $ string "at") 
                                           <* spaces
                         <*> parseTimeOfDay

          timeFirst = flip (,) <$> parseTimeOfDay <* spaces 
                                                  <* optional (try $ string "on")
                                                  <* spaces
                               <*> dayOfWeekWord

monthWord :: Stream s m Char => ParsecT s u m Int
monthWord = fromJust . flip lookup monthWordToInt <$> minDistance 4 monthWords

slashDate :: Stream s m Char => ParsecT s u m (Int, Int, Int)
slashDate = (,,) <$> parseNatInRange 1 12 <* char '/'
                 <*> parseNatInRange 1 31
                 <*> option 2012 (char '/' *> year)
    where year = (\n -> if n < 100 then 2000 + n else n) <$> positiveNatural

dateWords :: Stream s m Char => ParsecT s u m (Int, Int, Int)
dateWords 
