module Kassandra.Backend.Calendar (
  getEvents,
  newCache,
  Cache (..),
  setList,
  loadCache,
  saveCache,
) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (def))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (ZonedTime (ZonedTime), addDays, getCurrentTime, nominalDay, utc)
import Data.Time.Zones (
  TZ,
  loadLocalTZ,
  loadTZFromDB,
  localTimeToUTCTZ,
  timeZoneForUTCTime,
 )
import qualified StmContainers.Map as STM
import Streamly (IsStream, SerialT, async, asyncly, maxThreads)
import qualified Streamly.Prelude as S
import System.Directory (
  XdgDirectory (XdgCache),
  createDirectoryIfMissing,
  getModificationTime,
  getXdgDirectory,
  setModificationTime,
 )
import System.FilePath (dropFileName, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Text.ICalendar (
  DTEnd (DTEndDateTime, dtEndDateValue),
  DTStart (DTStartDateTime, dtStartDateValue),
  Date (dateValue),
  DateTime (FloatingDateTime, UTCDateTime, ZonedDateTime),
  Description (descriptionValue),
  Location (locationValue),
  OtherProperty (OtherProperty, otherName, otherValue),
  Summary (summaryValue),
  UID (uidValue),
  VCalendar (vcEvents),
  VEvent (
    veDTEndDuration,
    veDTStart,
    veDescription,
    veLocation,
    veOther,
    veSummary,
    veUID
  ),
  parseICalendarFile,
  printICalendar,
 )

import Control.Exception (onException)
import Data.Aeson (decodeStrict', encode)
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Kassandra.Calendar (
  CalendarEvent (..),
  CalendarList (CalendarList),
  EventTime (AllDayEvent, SimpleEvent),
  TZTime (TZTime),
  tzTimeToUTC,
  zonedDay,
 )
import Kassandra.Debug (Severity (..), log)
import qualified Streamly.Data.Fold as FL
import Streamly.External.ByteString (fromArray, toArray)
import qualified Streamly.FileSystem.Handle as FS
import qualified Streamly.Internal.FileSystem.File as FSFile
import Streamly.Memory.Array as Mem (fromList)
import Streamly.Internal.Data.Array.Stream.Foreign (splitOn)

dirName :: FilePath
dirName = "/home/maralorn/.calendars/"

data FileInfo = FileInfo
  { lastRead :: UTCTime
  , events :: Seq CalendarEvent
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

loadCache :: Cache -> IO ()
loadCache cache = do
  dir <- getCacheDir
  let a =
        S.drain $
          readJSONStream (cache ^. #icsCache) (dir </> "fileinfo.cache")
            `async` readJSONStream (cache ^. #uidCache) (dir </> "uid.cache")
  catch a \(e :: IOException) -> log Warning [i|Error loading calendar Cache:#{e}|]
  log Debug "Cache Loaded"

readJSONStream :: (IsStream t, Eq k, Hashable k, MonadIO (t IO), FromJSON k, FromJSON v) => STM.Map k v -> FilePath -> t IO ()
readJSONStream stmMap fileName =
  FSFile.withFile fileName ReadMode $
    S.fold
      (foldSTMMap stmMap)
      . asyncly
      . S.mapMaybe (decodeStrict' . fromArray)
      . splitOn 10
      . S.unfold FS.readChunks

saveCache :: Cache -> IO ()
saveCache cache = do
  dir <- getCacheDir
  createDirectoryIfMissing True dir
  let a =
        S.drain $
          writeJSONStream (cache ^. #icsCache) (dir </> "fileinfo.cache")
            `async` writeJSONStream (cache ^. #uidCache) (dir </> "uid.cache")
  catch a \(e :: IOException) -> log Warning [i|Error writing calendar Cache:#{e}|]
  log Debug "Saved Cache"

writeJSONStream :: (IsStream t, MonadIO (t IO), ToJSON k, ToJSON v) => STM.Map k v -> FilePath -> t IO ()
writeJSONStream stmMap fileName =
  FSFile.withFile fileName WriteMode \handle ->
    liftIO $
      S.fold (FS.writeChunks handle)
        . asyncly
        . S.intersperse (Mem.fromList [10])
        . fmap (toArray . toStrict . encode)
        $ streamSTMMap stmMap

streamSTMMap :: forall t k v. (MonadIO (t IO), IsStream t) => STM.Map k v -> t IO (k, v)
streamSTMMap = join . atomically . UnfoldlM.foldlM' (\x y -> pure $ S.cons y x) S.nil . STM.unfoldlM

getCacheDir :: IO FilePath
getCacheDir = getXdgDirectory XdgCache "kassandra"

foldSTMMap :: forall m k v. (Eq k, Hashable k, MonadIO m) => STM.Map k v -> FL.Fold m (k, v) ()
foldSTMMap cache = FL.foldMapM (\(k, v) -> atomically $ STM.insert v k cache)

type ICSCache = STM.Map FilePath FileInfo
type UIDCache = STM.Map Text FilePath
type TZCache = STM.Map (Maybe Text) (Maybe (Text, TZ))

data Cache = Cache
  { icsCache :: ICSCache
  , tzCache :: TZCache
  , uidCache :: UIDCache
  } deriving (Generic)

newCache :: IO Cache
newCache = atomically $ Cache <$> STM.new <*> STM.new <*> STM.new

makeLabels ''Cache

foldToSeq :: Monad m => SerialT m a -> m (Seq a)
foldToSeq = S.foldl' (|>) mempty

setList :: Cache -> Text -> CalendarList -> IO ()
setList cache uid list = do
  cachedFilename <- atomically (STM.lookup uid (cache ^. #uidCache))
  cachedFilename & maybe
    (log Error [i|Missing uid #{uid} in UIDCache.|])
    \filename -> do
      calendars <- foldToSeq (readCalendars filename)
      insertList calendars & maybe
        (log Error [i|Did not find Event with uid #{uid} in #{filename}.|])
        \newCalendars -> do
          withFile filename WriteMode \fileHandle -> do
            forM_ newCalendars (LBS.hPut fileHandle . printICalendar def)
      now <- getCurrentTime
      setModificationTime (dropFileName filename) now
 where
  insertList :: Traversable m => m VCalendar -> Maybe (m VCalendar)
  insertList cals = if modified then Just ret else Nothing
   where
    (ret, modified) = runState (insertListS cals) False
  insertListS :: Traversable m => m VCalendar -> State Bool (m VCalendar)
  insertListS = mapM \calendar -> do
    newEvents <- forM (vcEvents calendar) \event ->
      if uidValue (veUID event) == toLazy uid
        then
          event
            { veOther =
                Set.insert (OtherProperty tasksFieldName (maskICSText $ JSON.encode list) def)
                  . Set.filter (not . isTasksOther)
                  . veOther
                  $ event
            }
            <$ put True
        else pure event
    pure calendar{vcEvents = newEvents}

maskICSText :: LByteString -> LByteString
maskICSText = LBS.concatMap \case
  59 -> "\\;"
  10 -> "\\n"
  44 -> "\\,"
  92 -> "\\\\"
  c -> one c

unmaskICSText :: LByteString -> LByteString
unmaskICSText = maybe "" (uncurry f) . LBS.uncons
 where
  f 92 rest = maybe "" (uncurry w) (LBS.uncons rest)
  f x rest = one x <> unmaskICSText rest
  w 92 rest = "\\" <> unmaskICSText rest
  w 110 rest = "\n" <> unmaskICSText rest
  w 44 rest = "," <> unmaskICSText rest
  w 59 rest = ";" <> unmaskICSText rest
  w _ rest = unmaskICSText rest

tasksFieldName :: IsString t => t
tasksFieldName = "X-KASSANDRA-TASKS"

isTasksOther :: OtherProperty -> Bool
isTasksOther = (== tasksFieldName) . otherName

getEvents :: (MonadIO (stream IO), IsStream stream) => Cache -> stream IO CalendarEvent
getEvents cache = do
  now <- liftIO getZonedTime
  let calendarFiles = (dirName </>) <$> (S.fromList =<< liftIO (getDirectoryFiles dirName ["**/*.ics"]))
      extractEvents = getWithCache cache (zonedTimeToUTC now)
      filterRelevant = S.filter (onlyNextWeek now)
  maxThreads 500 . filterRelevant $ extractEvents =<< calendarFiles

getWithCache :: (MonadIO (stream IO), IsStream stream) => Cache -> UTCTime -> FilePath -> stream IO CalendarEvent
getWithCache cache now fileName =
  S.fromFoldable =<< liftIO do
    infoMay <- atomically $ STM.lookup fileName (cache ^. #icsCache)
    let update = do
          newEvents <- foldToSeq (readEvents cache fileName)
          atomically $ STM.insert (FileInfo now newEvents) fileName (cache ^. #icsCache)
          pure newEvents
    infoMay & maybe update \FileInfo{lastRead, events} -> do
      modTime <- getModificationTime fileName
      if lastRead >= modTime then pure events else update

readCalendars :: (MonadIO (stream IO), IsStream stream) => FilePath -> stream IO VCalendar
readCalendars path =
  S.fromList
    =<< liftIO
      ( parseICalendarFile def path >>= \case
          Right (calendarsInFile, fmap toText -> _warnings) -> do
            mapM_ (log Debug) _warnings
            pure calendarsInFile
          Left _parseError -> do
            log Debug (toText _parseError)
            pure mempty
      )

readEvents :: (MonadIO (stream IO), IsStream stream) => Cache -> FilePath -> stream IO CalendarEvent
readEvents cache path = do
  calendar <- readCalendars path
  let uids = toStrict . fst <$> (Map.keys . vcEvents) calendar
  atomically $ forM_ uids \uid -> STM.insert path uid (cache ^. #uidCache)
  translateCalendar cache (tryExtractBaseDir (toText path)) calendar

tryExtractBaseDir :: Text -> Text
tryExtractBaseDir name = fromMaybe name . (viaNonEmpty last <=< viaNonEmpty init) . Text.splitOn "/" $ name

onlyNextWeek :: ZonedTime -> CalendarEvent -> Bool
onlyNextWeek (zonedTimeToUTC -> now) CalendarEvent{time}
  | SimpleEvent (tzTimeToUTC -> start) (tzTimeToUTC -> end) <- time = end >= now && start <= addUTCTime (14 * nominalDay) now
onlyNextWeek now CalendarEvent{time}
  | AllDayEvent startDay endDay <- time = endDay >= zonedDay now && startDay <= addDays 14 (zonedDay now)
onlyNextWeek _ _ = False

translateCalendar :: (Monad (stream IO), IsStream stream) => Cache -> Text -> VCalendar -> stream IO CalendarEvent
translateCalendar cache calendarName = translateEvent cache calendarName <=< S.fromList . Map.elems . vcEvents

translateEvent :: (Functor (stream IO), IsStream stream) => Cache -> Text -> VEvent -> stream IO CalendarEvent
translateEvent cache calendarName vEvent =
  withTime <$> getTimes
 where
  withTime time = CalendarEvent{uid, description, todoList, time, calendarName, location, comment}
   where
    uid = toStrict (uidValue (veUID vEvent))
    description = (maybe "" (toStrict . summaryValue) . veSummary) vEvent
    todoList = find isTasksOther (veOther vEvent) >>= JSON.decode . unmaskICSText . otherValue & fromMaybe (CalendarList mempty mempty)
    location = toStrict . locationValue <$> veLocation vEvent
    comment = toStrict . descriptionValue <$> veDescription vEvent
  -- TODO: This currently misses recurring events and
  -- events with a duration configured
  -- Also we are ignoring the timezone delivered with this calendar and taking our own
  getTimes
    | Just (DTStartDateTime start _) <- veDTStart vEvent
      , Just (Left (DTEndDateTime end _)) <- veDTEndDuration vEvent =
      S.yieldM . liftIO $ SimpleEvent <$> datetimeToTZTime cache start <*> datetimeToTZTime cache end
    | Just (dateValue . dtStartDateValue -> start) <- veDTStart vEvent
      , Just (Left (dateValue . dtEndDateValue -> end)) <- veDTEndDuration vEvent =
      S.yield $ AllDayEvent start (addDays (-1) end)
    | otherwise = S.nil

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
nextName = fmap (Text.intercalate "/") . viaNonEmpty tail . filter (not . Text.null) . Text.splitOn "/"
