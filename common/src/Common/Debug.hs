{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Common.Debug
  ( log
  , logShow
  , logR
  , logRShow
  , setLogLevel
  , Severity(..)
  )
where

import           GHC.Stack                      ( srcLocStartLine
                                                , srcLocModule
                                                , callStack
                                                , SrcLoc(SrcLoc)
                                                )
import           Reflex                        as R
import           System.IO.Unsafe               ( unsafePerformIO )
import           Control.Concurrent             ( modifyMVar
                                                , ThreadId
                                                , myThreadId
                                                )

import qualified Debug.Trace                   as Trace
import           System.Console.ANSI            ( Color(..)
                                                , ColorIntensity(Vivid)
                                                , ConsoleLayer(Foreground)
                                                , SGR(..)
                                                , setSGRCode
                                                )


data Severity = Debug | Info | Warning | Error deriving (Show, Read, Eq, Ord)

class ReflexLoggable l a | l -> a where
  useLogString :: (Text -> a -> Text) -> l -> l

instance R.Reflex t => ReflexLoggable (R.Dynamic t a) a where
  useLogString f d =
    let e'    = traceEventWith (toString . f "updated Dynamic") $ updated d
        getV0 = do
          x <- sample $ current d
          Trace.trace (toString $ f "initialized Dynamic" x) $ return x
    in  unsafeBuildDynamic getV0 e'

instance R.Reflex t => ReflexLoggable (R.Event t a) a where
  useLogString f e = traceEventWith (toString . f "triggered Event") e

logR
  :: (HasCallStack, MonadIO m, ReflexLoggable l a)
  => Severity
  -> (a -> Text)
  -> l
  -> m l
logR severity decorate loggable = do
  isSevere <- severeEnough severity
  if isSevere
    then do
      myId <- liftIO $ modifyMVar traceID $ \a -> pure (succ a, a)
      withFrozenCallStack $ log Debug ("Registering eventTrace " <> show myId)
      let f comment value = formatMessage Message
            { msgSeverity  = severity
            , msgCallStack = callStack
            , msgThreadId  = unsafePerformIO myThreadId
            , msgTime      = unsafePerformIO getZonedTime
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
logRShow a = withFrozenCallStack (logR a show)

logShow :: (HasCallStack, Show a, MonadIO m) => Severity -> a -> m ()
logShow s = withFrozenCallStack (log s . show)

log :: (HasCallStack, MonadIO m) => Severity -> Text -> m ()
log severity text = do
  thread <- liftIO myThreadId
  time   <- liftIO getZonedTime
  whenM (severeEnough severity) . putTextLn . formatMessage $ Message
    { msgSeverity  = severity
    , msgCallStack = callStack
    , msgThreadId  = thread
    , msgTime      = time
    , msgComment   = Nothing
    , msgContent   = text
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

data Message = Message {
 msgSeverity :: !Severity,
 msgCallStack :: !CallStack,
 msgThreadId :: !ThreadId,
 msgTime :: !ZonedTime,
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

square :: Text -> Text
square t = "[" <> t <> "] "

-- [30 May 2020 16:44:03.534 +00:00]
showTime :: ZonedTime -> Text
showTime = square . toText . formatTime defaultTimeLocale "%F %T%3Q %z"

{- | Formats severity in different colours with alignment.
-}
showSeverity :: Severity -> Text
showSeverity = \case
  Debug   -> color Green "[Debug]   "
  Info    -> color Blue "[Info]    "
  Warning -> color Yellow "[Warning] "
  Error   -> color Red "[Error]   "
 where
  color :: Color -> Text -> Text
  color c txt =
    toText (setSGRCode [SetColor Foreground Vivid c]) <> txt <> toText
      (setSGRCode [Reset])

showSourceLoc :: CallStack -> Text
showSourceLoc cs = square showCallStack
 where
  showCallStack :: Text
  showCallStack = case getCallStack cs of
    [] -> "<unknown loc>"
    [(name, loc)] -> showLoc name loc
    (_, loc) : (callerName, _) : _ -> showLoc callerName loc

  showLoc :: String -> SrcLoc -> Text
  showLoc name SrcLoc { srcLocModule, srcLocStartLine, ..} =
    toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine
