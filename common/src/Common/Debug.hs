{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Common.Debug
  ( log
  , logShow
  , logR
  , logRShow
  , setLogLevel
  , pattern D
  , pattern I
  , pattern W
  , pattern E
  )
where

import           GHC.Stack                      ( callStack )
import           Reflex                        as R
import           Colog                          ( showSourceLoc
                                                , showSeverity
                                                , Severity
                                                , pattern D
                                                , pattern I
                                                , pattern W
                                                , pattern E
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Control.Concurrent             ( modifyMVar
                                                , ThreadId
                                                , myThreadId
                                                )

import qualified Data.Text.Lazy.Builder        as TB
import qualified Data.Text.Lazy.Builder.Int    as TB
import qualified Chronos.Locale.English        as C
import qualified Chronos                       as C
import qualified Data.Vector                   as Vector

class ReflexLoggable l a | l -> a where
  useLogString :: (Text -> a -> Text) -> l -> l

instance R.Reflex t => ReflexLoggable (R.Dynamic t a) a where
  useLogString f d =
    let e'    = traceEventWith (toString . f "updated Dynamic") $ updated d
        getV0 = do
          x <- sample $ current d
          trace (toString $ f "initialized Dynamic" x) $ return x
    in  unsafeBuildDynamic getV0 e'
instance R.Reflex t => ReflexLoggable (R.Event t a) a where
  useLogString f e = traceEventWith (toString . f "triggered Event") e

logLevel :: MVar (Maybe Severity)
logLevel = unsafePerformIO . newMVar $ Just W

traceID :: MVar Int
traceID = unsafePerformIO . newMVar $ 0

setLogLevel :: Maybe Severity -> IO ()
setLogLevel = void . swapMVar logLevel

logR
  :: (HasCallStack, MonadIO m, ReflexLoggable l a)
  => Severity
  -> l
  -> (a -> Text)
  -> m l
logR severity loggable decorate = do
  isSevere <- severeEnough severity
  if isSevere
    then do
      myId <- liftIO $ modifyMVar traceID $ \a -> pure (succ a, a)
      withFrozenCallStack $ log D ("Registering eventTrace " <> show myId)
      let f comment value = formatMessage Message
            { msgSeverity  = severity
            , msgCallStack = callStack
            , msgThreadId  = unsafePerformIO myThreadId
            , msgTime      = unsafePerformIO C.now
            , msgComment   = Just (comment <> " " <> show myId)
            , msgContent   = decorate value
            }
      pure $ useLogString f loggable
    else pure loggable

logRShow
  :: (HasCallStack, MonadIO m, ReflexLoggable l a, Show a)
  => Severity
  -> l
  -> m l
logRShow a b = withFrozenCallStack (logR a b show)

logShow :: (HasCallStack, Show a, MonadIO m) => Severity -> a -> m ()
logShow s = withFrozenCallStack (log s . show)

log :: (HasCallStack, MonadIO m) => Severity -> Text -> m ()
log severity text = do
  thread <- liftIO myThreadId
  time   <- liftIO C.now
  whenM (severeEnough severity) . putTextLn . formatMessage $ Message
    { msgSeverity  = severity
    , msgCallStack = callStack
    , msgThreadId  = thread
    , msgTime      = time
    , msgComment   = Nothing
    , msgContent   = text
    }

severeEnough :: MonadIO m => Severity -> m Bool
severeEnough severity = severeEnough' <$> readMVar logLevel
 where
  severeEnough' (Just minSeverity) | minSeverity <= severity = True
  severeEnough' _ = False

data Message = Message {
 msgSeverity :: !Severity,
 msgCallStack :: !CallStack,
 msgThreadId :: !ThreadId,
 msgTime :: !C.Time,
 msgComment :: !(Maybe Text),
 msgContent :: !Text }

formatMessage :: Message -> Text
formatMessage Message {..} =
  showSeverity msgSeverity
    <> showTime msgTime
    <> showSourceLoc msgCallStack
    <> square (show msgThreadId)
    <> maybe "" square msgComment
    <> msgContent

--------------------
-- Pastes from Colog

square :: Text -> Text
square t = "[" <> t <> "] "

showTime :: C.Time -> Text
showTime t =
  square $ toStrict $ TB.toLazyText $ builderDmyHMSz (C.timeToDatetime t)


----------------------------------------------------------------------------
-- Chronos extra
----------------------------------------------------------------------------

{- | Given a 'Datetime', constructs a 'Text' 'TB.Builder' corresponding to a
Day\/Month\/Year,Hour\/Minute\/Second\/Offset encoding of the given 'Datetime'.

Example: @29 Dec 2019 22:00:00.000 +00:00@
-}
builderDmyHMSz :: C.Datetime -> TB.Builder
builderDmyHMSz (C.Datetime date time) =
  builderDmy date
    <> spaceSep
    <> C.builder_HMS (C.SubsecondPrecisionFixed 3) (Just ':') time
    <> spaceSep
    <> C.builderOffset C.OffsetFormatColonOn (C.Offset 0)
 where
  spaceSep :: TB.Builder
  spaceSep = TB.singleton ' '

  {- | Given a 'Date' construct a 'Text' 'TB.Builder'
    corresponding to a Day\/Month\/Year encoding.

    Example: @01 Jan 2020@
    -}
  builderDmy :: C.Date -> TB.Builder
  builderDmy (C.Date (C.Year y) m d) =
    zeroPadDayOfMonth d
      <> spaceSep
      <> TB.fromText (C.caseMonth C.abbreviated m)
      <> spaceSep
      <> TB.decimal y


  zeroPadDayOfMonth :: C.DayOfMonth -> TB.Builder
  zeroPadDayOfMonth (C.DayOfMonth d) =
    if d < 100 then Vector.unsafeIndex twoDigitTextBuilder d else TB.decimal d

  twoDigitTextBuilder :: Vector.Vector TB.Builder
  twoDigitTextBuilder =
    Vector.fromList $ map (TB.fromText . toText) twoDigitStrings
  {-# NOINLINE twoDigitTextBuilder #-}

  twoDigitStrings :: [String]
  twoDigitStrings =
    [ "00"
    , "01"
    , "02"
    , "03"
    , "04"
    , "05"
    , "06"
    , "07"
    , "08"
    , "09"
    , "10"
    , "11"
    , "12"
    , "13"
    , "14"
    , "15"
    , "16"
    , "17"
    , "18"
    , "19"
    , "20"
    , "21"
    , "22"
    , "23"
    , "24"
    , "25"
    , "26"
    , "27"
    , "28"
    , "29"
    , "30"
    , "31"
    , "32"
    , "33"
    , "34"
    , "35"
    , "36"
    , "37"
    , "38"
    , "39"
    , "40"
    , "41"
    , "42"
    , "43"
    , "44"
    , "45"
    , "46"
    , "47"
    , "48"
    , "49"
    , "50"
    , "51"
    , "52"
    , "53"
    , "54"
    , "55"
    , "56"
    , "57"
    , "58"
    , "59"
    , "60"
    , "61"
    , "62"
    , "63"
    , "64"
    , "65"
    , "66"
    , "67"
    , "68"
    , "69"
    , "70"
    , "71"
    , "72"
    , "73"
    , "74"
    , "75"
    , "76"
    , "77"
    , "78"
    , "79"
    , "80"
    , "81"
    , "82"
    , "83"
    , "84"
    , "85"
    , "86"
    , "87"
    , "88"
    , "89"
    , "90"
    , "91"
    , "92"
    , "93"
    , "94"
    , "95"
    , "96"
    , "97"
    , "98"
    , "99"
    ]
