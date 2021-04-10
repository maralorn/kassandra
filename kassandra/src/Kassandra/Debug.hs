module Kassandra.Debug (
  log,
  logShow,
  logR,
  logRShow,
  setLogLevel,
  Severity (..),
) where

import Control.Concurrent (
  ThreadId,
  modifyMVar,
  myThreadId,
 )
import GHC.Stack (
  SrcLoc (SrcLoc),
  srcLocModule,
  srcLocStartLine,
 )
import Reflex as R
import System.IO.Unsafe (unsafePerformIO)

import qualified Debug.Trace as Trace
import System.Console.ANSI (
  Color (..),
  ColorIntensity (Vivid),
  ConsoleLayer (Foreground),
  SGR (..),
  setSGRCode,
 )
import Relude.Extra.Bifunctor
import Relude.Extra.Enum
import Say

data Severity = Debug | Info | Warning | Error deriving stock (Show, Read, Eq, Ord)

class ReflexLoggable l where
  useLogString :: (Text -> a -> Text) -> l a -> l a

instance R.Reflex t => ReflexLoggable (R.Dynamic t) where
  useLogString f d =
    let e' = traceEventWith (toString . f "updated Dynamic") $ updated d
        getV0 = do
          x <- sample $ current d
          Trace.trace (toString $ f "initialized Dynamic" x) $ return x
     in unsafeBuildDynamic getV0 e'

instance R.Reflex t => ReflexLoggable (R.Event t) where
  useLogString f e = traceEventWith (toString . f "triggered Event") e

logR ::
  (HasCallStack, MonadIO m, ReflexLoggable l) =>
  Severity ->
  (a -> Text) ->
  l a ->
  m (l a)
logR severity decorate loggable = do
  isSevere <- severeEnough severity
  if isSevere
    then do
      myId <- liftIO $ modifyMVar traceID $ \a -> pure (next a, a)
      withFrozenCallStack $ log Debug ("Registering eventTrace " <> show myId)
      let f comment value =
            formatMessage
              Message
                { msgSeverity = severity
                , msgCallStack = callStack
                , msgThreadId = unsafePerformIO myThreadId
                , msgTime = unsafePerformIO getZonedTime
                , msgComment = Just (comment <> " " <> show myId)
                , msgContent = decorate value
                }
      pure $ useLogString f loggable
    else pure loggable

logRShow ::
  (HasCallStack, MonadIO m, ReflexLoggable l, Show a) =>
  Severity ->
  l a ->
  m (l a)
logRShow a = withFrozenCallStack (logR a show)

logShow :: (HasCallStack, Show a, MonadIO m) => Severity -> a -> m ()
logShow s = withFrozenCallStack (log s . show)

log :: (HasCallStack, MonadIO m) => Severity -> Text -> m ()
log severity text = do
  thread <- liftIO myThreadId
  time <- liftIO getZonedTime
  whenM (severeEnough severity) . say . formatMessage $
    Message
      { msgSeverity = severity
      , msgCallStack = callStack
      , msgThreadId = thread
      , msgTime = time
      , msgComment = Nothing
      , msgContent = text
      }

{-# NOINLINE logLevel #-}
logLevel :: MVar (Maybe Severity)
logLevel = unsafePerformIO . newMVar $ Just Warning

{-# NOINLINE traceID #-}
traceID :: MVar Int
traceID = unsafePerformIO . newMVar $ 0

setLogLevel :: Maybe Severity -> IO ()
setLogLevel = void . swapMVar logLevel

severeEnough :: MonadIO m => Severity -> m Bool
severeEnough severity = severeEnough' <$> readMVar logLevel
 where
  severeEnough' (Just minSeverity) | minSeverity <= severity = True
  severeEnough' _ = False

data Message = Message
  { msgSeverity :: !Severity
  , msgCallStack :: !CallStack
  , msgThreadId :: !ThreadId
  , msgTime :: !ZonedTime
  , msgComment :: !(Maybe Text)
  , msgContent :: !Text
  }

formatMessage :: Message -> Text
formatMessage Message{msgSeverity, msgTime, msgCallStack, msgThreadId, msgComment, msgContent} =
  showSeverity msgSeverity
    <> showTime msgTime
    <> showSourceLoc msgCallStack
    <> square (show msgThreadId)
    <> maybe "" square msgComment
    <> msgContent

square :: Text -> Text
square t = "[" <> t <> "] "

-- [30 May 2020 16:44:03.534 +00:00]
showTime :: ZonedTime -> Text
showTime = square . toText . formatTime defaultTimeLocale "%F %T%3Q %z"

-- | Formats severity in different colours with alignment.
showSeverity :: Severity -> Text
showSeverity = \case
  Debug -> color Green "[Debug]   "
  Info -> color Blue "[Info]    "
  Warning -> color Yellow "[Warning] "
  Error -> color Red "[Error]   "
 where
  color :: Color -> Text -> Text
  color c txt =
    toText (setSGRCode [SetColor Foreground Vivid c]) <> txt
      <> toText
        (setSGRCode [Reset])

showSourceLoc :: CallStack -> Text
showSourceLoc = square . showCallStack . firstF toText . getCallStack
 where
  showCallStack = \case
    [] -> "<unknown loc>"
    [(name, SrcLoc{srcLocModule, srcLocStartLine})] ->
      name <> "@" <> toText srcLocModule <> "#" <> show srcLocStartLine
    (_, SrcLoc{srcLocModule, srcLocStartLine}) : (callerName, _) : _ ->
      toText srcLocModule <> "." <> callerName <> "#" <> show srcLocStartLine
