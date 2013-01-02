--import Text.ParserCombinators.Parsec
import ParseCommon
import ParseDateTime
import Data.Time.LocalTime
import Data.Char (isSpace, toUpper)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Maybe
import Control.Monad (fail)
import qualified Data.Set as S
import System.IO.Unsafe (unsafePerformIO)

type Parser = Parsec String ()

data Event = Event {
    evtTime :: ZonedTime, 
    evtName :: String,
    evtLoc :: Maybe Location
}

data Location = Loc {
    addressNum :: String, -- should it be a string or an int?
    streetName :: String,
    aptNum :: String
} deriving (Show)

parseLocation :: Parser Location
parseLocation = Loc <$> many digit <* spaces
                    <*> parseStreetName <* spaces
                    <*> option "" (try parseApt)

parseApt :: Parser String
parseApt = do
    string "apt" <|> string "Apt"
    optional $ char '.'
    spaces
    optional $ char '#'
    many digit

parseStreetName :: Parser String
parseStreetName = unwords <$> (manyTillE (word <* spaces) $ try parseStreetSuffix)

{-
    Types of event strings:
        - NAME (at LOC) NAME (on/at time) NAME
        - NAME (on/at time) NAME (at LOC) NAME
            where NAME = many word
-}

streetSuffixes :: S.Set String
streetSuffixes = S.fromList . lines . unsafePerformIO $ readFile "streets.txt"

succeedIf :: (a -> Bool) -> Parser a -> Parser a
succeedIf f p = p >>= (\x -> if f x then return x else fail "predicate failed")

parseStreetSuffix :: Parser String
parseStreetSuffix = succeedIf ((`S.member` streetSuffixes) . map toUpper) word


--parseEvt = try parseEvt1 <|> parseEvt2

--parseEvt1 :: Parser Event
parseEvt1 = do
    name1 <- manyTill (word <* spaces) $ try (string "at")
    spaces
    addr <- parseLocation
    string "on" <|> string "at"
    spaces
    time <- weekdayAndTime
    return (name1, addr, time)



--    option defaultTime
