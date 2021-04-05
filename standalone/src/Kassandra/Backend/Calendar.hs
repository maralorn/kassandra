module Kassandra.Backend.Calendar (
  getEvents,
  newCache,
  Cache (..),
  setList,
) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
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
import Kassandra.Debug (Severity (Debug, Error, Info), log)
import UnliftIO (onException)

dirName :: FilePath
dirName = "/home/maralorn/.calendars/"

data FileInfo = FileInfo
  { lastRead :: UTCTime
  , events :: Seq CalendarEvent
  }
  deriving stock (Show, Generic)

type ICSCache = STM.Map FilePath FileInfo
type UIDCache = STM.Map Text FilePath
type TZCache = STM.Map (Maybe Text) (Maybe (Text, TZ))

data Cache = Cache
  { icsCache :: ICSCache
  , tzCache :: TZCache
  , uidCache :: UIDCache
  }

newCache :: IO Cache
newCache = atomically $ Cache <$> STM.new <*> STM.new <*> STM.new

makeLabels ''Cache

setList :: Cache -> Text -> CalendarList -> IO ()
setList cache uid list = do
  cachedFilename <- atomically (STM.lookup uid (cache ^. #uidCache))
  cachedFilename & maybe
    (log Error [i|Missing uid #{uid} in UIDCache.|])
    \filename -> do
      calendars <- readCalendars filename
      insertList calendars & maybe
        (log Error [i|Did not find Event with uid #{uid} in #{filename}.|])
        \newCalendars -> do
          withFile filename WriteMode \fileHandle -> do
            forM_ newCalendars (LBS.hPut fileHandle . printICalendar def)
 where
  insertList :: Seq VCalendar -> Maybe (Seq VCalendar)
  insertList cals = if modified then Just ret else Nothing
   where
    (ret, modified) = runState (insertListS cals) False
  insertListS :: Seq VCalendar -> State Bool (Seq VCalendar)
  insertListS = mapM \calendar -> do
    newEvents <- forM (vcEvents calendar) \event ->
      if uidValue (veUID event) == toLazy uid
        then
          event
            { veOther =
                Set.insert (OtherProperty tasksFieldName (JSON.encode list) def)
                  . Set.filter (not . isTasksOther)
                  . veOther
                  $ event
            }
            <$ put True
        else pure event
    pure calendar{vcEvents = newEvents}

tasksFieldName :: IsString t => t
tasksFieldName = "TASKS"

isTasksOther :: OtherProperty -> Bool
isTasksOther = (== tasksFieldName) . otherName

getEvents :: Cache -> IO (Seq CalendarEvent)
getEvents cache = do
  calendarFiles <- fromList . fmap (dirName </>) <$> getDirectoryFiles dirName ["**/*.ics"]
  now <- getZonedTime
  log Info "Retrieving ics events"
  allEvents <- join <$> pooledForConcurrentlyN 1000 calendarFiles (getWithCache cache (zonedTimeToUTC now))
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

readCalendars :: FilePath -> IO (Seq VCalendar)
readCalendars path =
  parseICalendarFile def path >>= \case
    Right (fromList -> calendarsInFile, fmap toText -> _warnings) -> do
      mapM_ (log Debug) _warnings
      pure calendarsInFile
    Left _parseError -> do
      log Debug (toText _parseError)
      pure mempty

readEvents :: Cache -> FilePath -> IO (Seq CalendarEvent)
readEvents cache path = do
  calendars <- readCalendars path
  join <$> forM calendars \calendar -> do
    let uids = toStrict . fst <$> (Map.keys . vcEvents) calendar
    atomically do
      forM_ uids \uid -> STM.insert path uid (cache ^. #uidCache)
    translateCalendar cache (tryExtractBaseDir (toText path)) calendar

tryExtractBaseDir :: Text -> Text
tryExtractBaseDir name = fromMaybe name . (viaNonEmpty last <=< viaNonEmpty init) . Text.splitOn "/" $ name

onlyNextWeek :: ZonedTime -> CalendarEvent -> Bool
onlyNextWeek (zonedTimeToUTC -> now) CalendarEvent{time}
  | SimpleEvent (tzTimeToUTC -> start) (tzTimeToUTC -> end) <- time = end >= now && start <= addUTCTime (14 * nominalDay) now
onlyNextWeek now CalendarEvent{time}
  | AllDayEvent startDay endDay <- time = endDay >= zonedDay now && startDay <= addDays 14 (zonedDay now)
onlyNextWeek _ _ = False

translateCalendar :: Cache -> Text -> VCalendar -> IO (Seq CalendarEvent)
translateCalendar cache calendarName = fmap join . traverse (translateEvent cache calendarName) . fromList . Map.elems . vcEvents

translateEvent :: Cache -> Text -> VEvent -> IO (Seq CalendarEvent)
translateEvent cache calendarName vEvent =
  withTime <<$>> getTimes
 where
  withTime time = CalendarEvent{uid, description, todoList, time, calendarName, location, comment}
   where
    uid = toStrict (uidValue (veUID vEvent))
    description = (maybe "" (toStrict . summaryValue) . veSummary) vEvent
    todoList = find isTasksOther (veOther vEvent) >>= JSON.decode . otherValue & fromMaybe (CalendarList mempty mempty)
    location = toStrict . locationValue <$> veLocation vEvent
    comment = toStrict . descriptionValue <$> veDescription vEvent
  -- TODO: This currently misses recurring events and
  -- events with a duration configured
  -- Also we are ignoring the timezone delivered with this calendar and taking our own
  getTimes
    | Just (DTStartDateTime start _) <- veDTStart vEvent
      , Just (Left (DTEndDateTime end _)) <- veDTEndDuration vEvent =
      (one .) . SimpleEvent <$> datetimeToTZTime cache start <*> datetimeToTZTime cache end
    | Just (dateValue . dtStartDateValue -> start) <- veDTStart vEvent
      , Just (Left (dateValue . dtEndDateValue -> end)) <- veDTEndDuration vEvent =
      pure . one $ AllDayEvent start (addDays (-1) end)
    | otherwise = pure mempty

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
  ( tzMay & flip maybe pure do
      newTz <- maybe (("Your Time",) <$> loadLocalTZ) loadTZ tzname
      atomically $ STM.insert (Just newTz) tzname cache
      pure newTz
    )
    `onException` atomically (STM.delete tzname cache)
 where
  loadTZ name =
    ((name,) <$> loadTZFromDB (toString name)) `catch` \(e :: IOException) -> do
      log Debug [i|Timezone not found "#{name}" trying next.|]
      log Debug [i|Timezone lookup error was: #{e}|]
      getTZ cache (nextName name)

mkTZTime :: (Text, TZ) -> LocalTime -> TZTime
mkTZTime (tzname, tz) t = TZTime (ZonedTime t (timeZoneForUTCTime tz (localTimeToUTCTZ tz t))) tzname

nextName :: Text -> Maybe Text
nextName = fmap (Text.intercalate "/") . viaNonEmpty tail . Text.splitOn "/"
