module Kassandra.Backend.Calendar (
  getEvents,
  newCache,
  Cache (..),
) where

import qualified Control.Concurrent.STM as STM
import Data.Default
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Time
import Data.Time.Zones
import qualified StmContainers.Map as STM
import System.Directory
import System.FilePath
import System.FilePattern.Directory
import Text.ICalendar
import UnliftIO.Async

import Kassandra.Calendar
import Kassandra.Debug

dirName :: FilePath
dirName = "/home/maralorn/.calendars/"

data FileInfo = FileInfo
  { lastRead :: UTCTime
  , events :: Seq CalendarEvent
  }
  deriving stock (Show, Generic)

type ICSCache = STM.Map FilePath FileInfo
type TZCache = STM.Map (Maybe Text) (Maybe (Text, TZ))

data Cache = Cache
  { icsCache :: ICSCache
  , tzCache :: TZCache
  }

newCache :: IO Cache
newCache = atomically $ Cache <$> STM.new <*> STM.new

makeLabels ''Cache

getEvents :: Cache -> IO (Seq CalendarEvent)
getEvents cache = do
  calendarFiles <- fromList . fmap (dirName </>) <$> getDirectoryFiles dirName ["**/*.ics"]
  now <- getCurrentTime
  log Info "Retrieving ics events"
  allEvents <- join
    <$> pooledForConcurrentlyN 1000 calendarFiles (getWithCache cache now)
  log Info [i|Retrieved ics #{Seq.length allEvents} events.|]
  (pure . Seq.sortBy sortEvents . Seq.filter (onlyNextWeek now)) allEvents

getWithCache :: Cache -> UTCTime -> FilePath -> IO (Seq CalendarEvent)
getWithCache cache now fileName = do
  infoMay <- atomically $ STM.lookup fileName (cache ^. #icsCache)
  let update = do
        newEvents <- readEvents cache fileName
        atomically $ STM.insert (FileInfo now newEvents) fileName (cache ^. #icsCache)
        pure newEvents
  infoMay & maybe update \FileInfo{lastRead, events} -> do
    modTime <- getModificationTime fileName
    if lastRead >= modTime then pure events else update

readEvents :: Cache -> FilePath -> IO (Seq CalendarEvent)
readEvents cache path =
  parseICalendarFile def path >>= \case
    Right (fromList -> calendarsInFile, fmap toText -> _warnings) -> do
      mapM_ (log Debug) _warnings
      join <$> traverse (translateCalendar cache (tryExtractBaseDir (toText path))) calendarsInFile
    Left _parseError -> do
      log Debug (toText _parseError)
      pure mempty

tryExtractBaseDir :: Text -> Text
tryExtractBaseDir name = fromMaybe name . (viaNonEmpty last <=< viaNonEmpty init) . Text.splitOn "/" $ name

onlyNextWeek :: UTCTime -> CalendarEvent -> Bool
onlyNextWeek now CalendarEvent{time}
  | SimpleEvent (tzTimeToUTC -> start) (tzTimeToUTC -> end) <- time = end >= now && start <= addUTCTime (14 * nominalDay) now
onlyNextWeek _ _ = False

translateCalendar :: Cache -> Text -> VCalendar -> IO (Seq CalendarEvent)
translateCalendar cache calendarName = traverse (translateEvent cache calendarName) . fromList . Map.elems . vcEvents

translateEvent :: Cache -> Text -> VEvent -> IO CalendarEvent
translateEvent cache calendarName vEvent = do
  time <- getTime
  pure
    CalendarEvent
      { uid = toStrict (uidValue (veUID vEvent))
      , description = (maybe "" (toStrict . summaryValue) . veSummary) vEvent
      , todoList = mempty
      , time
      , calendarName
      }
 where
  getTime
    | Just (DTStartDateTime start _) <- veDTStart vEvent
      , Just (Left (DTEndDateTime end _)) <- veDTEndDuration vEvent =
      SimpleEvent <$> datetimeToTZTime cache start <*> datetimeToTZTime cache end
    | otherwise = pure RecurringEvent

datetimeToTZTime :: Cache -> DateTime -> IO TZTime
datetimeToTZTime cache = \case
  FloatingDateTime t -> withTZ t Nothing
  UTCDateTime t -> pure $ TZTime (utcToZonedTime utc t) "UTC"
  ZonedDateTime t (Just . toStrict -> tzname) -> withTZ t tzname
 where
  withTZ :: LocalTime -> Maybe Text -> IO TZTime
  withTZ t tzname =
    mkTZTime <$> getTZ (cache ^. #tzCache) tzname <*> pure t

getTZ :: TZCache -> Maybe Text -> IO (Text, TZ)
getTZ cache tzname = do
  tzMay <- atomically $ do
    cached <- STM.lookup tzname cache
    cached & \case
      Just (Just tz) -> pure (Just tz) -- Cache hit
      Just Nothing -> STM.retry -- Another thread is already getting this value, wait for it.
      Nothing -> STM.insert Nothing tzname cache >> pure Nothing -- We need to get this value
  tzMay & flip maybe pure do
    newTz <-
      tzname & maybe (("Your Time",) <$> loadLocalTZ) \tzname' ->
        ((tzname',) <$> loadTZFromDB (toString tzname')) `catch` \(e :: IOException) -> do
          log Info [i|Timezone not found "#{tzname'}" trying next.|]
          log Debug [i|Timezone lookup error was: #{e}|]
          getTZ cache (nextName tzname')
    atomically $ STM.insert (Just newTz) tzname cache
    pure newTz

mkTZTime :: (Text, TZ) -> LocalTime -> TZTime
mkTZTime (tzname, tz) t = TZTime (ZonedTime t (timeZoneForUTCTime tz (localTimeToUTCTZ tz t))) tzname

nextName :: Text -> Maybe Text
nextName = fmap (Text.intercalate "/") . viaNonEmpty tail . Text.splitOn "/"
