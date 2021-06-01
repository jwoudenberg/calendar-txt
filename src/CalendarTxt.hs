module CalendarTxt (main, test) where

import qualified Chronos
import Conduit (ConduitT, runConduit, (.|))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser
import qualified Data.Attoparsec.ByteString as PB
import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy
import qualified Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Function ((&))
import qualified Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified System.Exit
import qualified System.IO

main :: IO ()
main = do
  test
  lsp

lsp :: IO ()
lsp =
  runConduit $
    C.stdin
      .| newlineSeperatedJson
      .| C.map (\(c :: ()) -> Data.ByteString.Lazy.toStrict $ Aeson.encode c)
      .| C.print

newlineSeperatedJson :: (Monad m, Aeson.FromJSON a) => ConduitT Data.ByteString.ByteString a m ()
newlineSeperatedJson =
  C.concatMapAccum (incrementalParseJsonStep []) (PB.parse json)
    .| C.map
      ( \val ->
          case Aeson.fromJSON val of
            Aeson.Error err -> Left err
            Aeson.Success res -> Right res
      )
    .| C.concat

incrementalParseJsonStep ::
  [Aeson.Value] ->
  ByteString ->
  (ByteString -> PB.Result Aeson.Value) ->
  ((ByteString -> PB.Result Aeson.Value), [Aeson.Value])
incrementalParseJsonStep acc chunk continue =
  case continue chunk of
    PB.Fail rest _ _ -> incrementalParseJsonStep acc (rest <> chunk) (PB.parse (skipUntilNewline *> json))
    PB.Partial f -> (f, [])
    PB.Done rest val -> incrementalParseJsonStep (val : acc) (rest <> chunk) (PB.parse json)

skipUntilNewline :: PB.Parser ()
skipUntilNewline = PB.skipWhile (/= 10 {- \n -})

json :: PB.Parser Aeson.Value
json = PB.skipWhile (== 10 {- \n -}) *> Data.Aeson.Parser.json

test :: IO ()
test = do
  contents <- Data.Text.IO.readFile "calendar.txt"
  let parseResult = P.parseOnly parser contents
  case parseResult of
    Left err -> System.Exit.die err
    Right events -> B.hPutBuilder System.IO.stdout (prettyPrint events)

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
  deriving (Eq, Show)

instance Ord Time where
  time1 <= time2 = startAt time1 <= startAt time2

startAt :: Time -> Maybe Chronos.TimeOfDay
startAt AllDay = Nothing
startAt (StartsAt time) = Just time
startAt (Timeslot time _) = Just time

prettyPrint :: [Event] -> B.Builder
prettyPrint events = foldMap printDay (NonEmpty.groupAllWith day events)

printDay :: NonEmpty.NonEmpty Event -> B.Builder
printDay events =
  let date = day (NonEmpty.head events)
   in Chronos.builderUtf8_Ymd (Just '-') date
        <> " "
        <> printWeekNumber date
        <> " "
        <> printDayOfWeek date
        <> "  "
        <> printEvents (Data.List.sortOn time (NonEmpty.toList events))
        <> "\n"

printEvents :: [Event] -> B.Builder
printEvents events =
  printEvent <$> events
    & Data.List.intersperse " "
    & mconcat

printEvent :: Event -> B.Builder
printEvent event =
  printTime (time event)
    <> " "
    <> B.byteString (Data.Text.Encoding.encodeUtf8 (description event))
    <> "."

printTime :: Time -> B.Builder
printTime time =
  case time of
    AllDay -> ""
    StartsAt startTime -> printSingleTime startTime
    Timeslot startTime endTime -> printSingleTime startTime <> "-" <> printSingleTime endTime

printSingleTime :: Chronos.TimeOfDay -> B.Builder
printSingleTime time =
  let hours = Chronos.timeOfDayHour time
      minutes = Chronos.timeOfDayMinute time
   in printWithLeadingZero hours
        <> (if minutes == 0 then "" else ":" <> printWithLeadingZero minutes)

dateToDayOfWeek :: Chronos.Date -> Chronos.DayOfWeek
dateToDayOfWeek date =
  Chronos.Datetime date (Chronos.TimeOfDay 0 0 0)
    & Chronos.datetimeToDayOfWeek

printDayOfWeek :: Chronos.Date -> B.Builder
printDayOfWeek date =
  Chronos.caseDayOfWeek
    ( Chronos.buildDayOfWeekMatch
        "Sun"
        "Mon"
        "Tue"
        "Wed"
        "Thu"
        "Fri"
        "Sat"
    )
    (dateToDayOfWeek date)

printWeekNumber :: Chronos.Date -> B.Builder
printWeekNumber date =
  let weekNumber = dateToWeekNumber date
   in "w" <> printWithLeadingZero weekNumber

printWithLeadingZero :: Int -> B.Builder
printWithLeadingZero n =
  (if n < 10 then "0" else "") <> (B.intDec n)

-- | Get the week number based on the current date.
-- We use the ISO definition of week one: The week that has the first Thursday
-- of the year in it.
-- See: https://en.wikipedia.org/wiki/ISO_week_date#First_week
dateToWeekNumber :: Chronos.Date -> Int
dateToWeekNumber date =
  let dayOfYear =
        Chronos.dateToDay date
          & Chronos.dayToOrdinalDate
          & Chronos.ordinalDateDayOfYear
          & Chronos.getDayOfYear
          & (+) (-1) -- Make first day of the year be 0.
      daysTillThursdayThisWeek =
        getDayOfWeek (Chronos.thursday) - getDayOfWeek (dateToDayOfWeek date)
   in (dayOfYear + daysTillThursdayThisWeek) `div` 7
        + 1 -- Make first week of the year be 1.

getDayOfWeek :: Chronos.DayOfWeek -> Int
getDayOfWeek dayOfWeek =
  Chronos.caseDayOfWeek
    (Chronos.buildDayOfWeekMatch 6 0 1 2 3 4 5)
    dayOfWeek

parser :: P.Parser [Event]
parser = mconcat <$> P.sepBy' lineParser P.endOfLine

lineParser :: P.Parser [Event]
lineParser = do
  skipSpace
  day <- Chronos.parser_Ymd_lenient
  skipSpace
  _ <- P.many' weekOrDayInfo
  skipSpace
  events <-
    P.sepBy'
      (uncurry (Event day) <$> descriptionParser)
      (P.char '.' <* skipSpace)
  P.option () (P.skip (== '.'))
  pure events

weekOrDayInfo :: P.Parser ()
weekOrDayInfo = do
  word <- P.many1' (P.satisfy Char.isAlphaNum)
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
  skipSpace
  if isWeekOrDayInfo then pure () else fail "not week or day info"

timeParser :: P.Parser Time
timeParser =
  P.option AllDay $ do
    startTime <- timeOfDayParser
    skipSpace
    P.option () (P.skip (== '-'))
    skipSpace
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

descriptionParser :: P.Parser (Time, Text)
descriptionParser = do
  time <- timeParser
  skipSpace
  description <- P.takeWhile1 (P.notInClass ".\n\r")
  pure (time, description)

skipSpace :: P.Parser ()
skipSpace = P.skipWhile P.isHorizontalSpace
