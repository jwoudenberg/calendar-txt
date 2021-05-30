module CalendarTxt where

import qualified Chronos
import qualified Data.Attoparsec.Text as P
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text.IO

main :: IO ()
main = do
  contents <- Data.Text.IO.readFile "calendar.txt"
  print (P.parseOnly parser contents)

data Event = Event
  { day :: Chronos.Date,
    time :: Time,
    description :: Text
  }
  deriving (Show)

data Time
  = AllDay
  | StartsAt Chronos.TimeOfDay
  | Timeslot Chronos.TimeOfDay Chronos.TimeOfDay
  deriving (Show)

parser :: P.Parser [Event]
parser = mconcat <$> P.sepBy' lineParser P.endOfLine

lineParser :: P.Parser [Event]
lineParser = do
  P.skipSpace
  day <- Chronos.parser_Ymd_lenient
  P.skipSpace
  _ <- P.many' weekOrDayInfo
  P.skipSpace
  time <- timeParser
  P.skipSpace
  events <- P.sepBy' (Event day time <$> descriptionParser) (P.char '.')
  P.option () (P.skip (== '.'))
  pure events

weekOrDayInfo :: P.Parser ()
weekOrDayInfo = do
  word <- P.many1' P.letter
  let isWeekOrDayInfo =
        case Char.toLower <$> word of
          "mon" -> True
          "tue" -> True
          "wed" -> True
          "thu" -> True
          "fri" -> True
          "sat" -> True
          "sun" -> True
          'w' : rest -> all Char.isDigit rest
          _ -> False
  P.skipSpace
  if isWeekOrDayInfo then pure () else fail "not week or day info"

timeParser :: P.Parser Time
timeParser =
  P.option AllDay $ do
    startTime <- timeOfDayParser
    P.skipSpace
    P.option () (P.skip (== '-'))
    P.skipSpace
    P.option (StartsAt startTime) (Timeslot startTime <$> timeOfDayParser)

timeOfDayParser :: P.Parser Chronos.TimeOfDay
timeOfDayParser = do
  digits <- P.many1 P.digit
  P.option
    (timeFromUnseperatedDigits digits)
    (timeFromHoursAndMinutes digits <$> (P.char ':' *> P.many1 P.digit))

timeFromUnseperatedDigits :: [Char] -> Chronos.TimeOfDay
timeFromUnseperatedDigits digits =
  case digits of
    [] -> Chronos.TimeOfDay 0 0 0
    [_] -> Chronos.TimeOfDay (read digits) 0 0
    [_, _] -> Chronos.TimeOfDay (read digits) 0 0
    [h, m1, m2] -> Chronos.TimeOfDay (read [h]) (read [m1, m2]) 0
    h1 : h2 : m1 : m2 : _ -> Chronos.TimeOfDay (read [h1, h2]) (read [m1, m2]) 0

timeFromHoursAndMinutes :: [Char] -> [Char] -> Chronos.TimeOfDay
timeFromHoursAndMinutes hours minutes =
  Chronos.TimeOfDay (read hours) (read minutes) 0

descriptionParser :: P.Parser Text
descriptionParser =
  P.skipSpace *> P.takeWhile1 (P.notInClass ".\n\r") <* P.skipSpace
