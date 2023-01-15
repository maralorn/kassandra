module Kassandra.TimeWidgets (
  dateSelectionWidget,
) where

import Kassandra.BaseWidgets (
  button,
  icon,
  stateWidget,
 )
import Kassandra.TextEditWidget (editText)
import Kassandra.Types (
  StandardWidget,
  Widget,
  getTime,
 )
import qualified Reflex as R
import qualified Reflex.Dom as D

myFormatTime :: ZonedTime -> Text
myFormatTime = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

myParseTime :: Text -> Either String LocalTime
myParseTime ((^. unpacked) -> t) =
  maybeToRight ("'" <> t <> "' cannot be parsed as '%Y-%m-%d %H:%M'.")
    . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
    $ t

inputDateWidget ::
  forall t m. Widget t m => ZonedTime -> m (R.Event t (Maybe ZonedTime))
inputDateWidget time = do
  textMayEvent <-
    editText @t @m . myFormatTime $ time :: m (R.Event t (Maybe Text))
  let cancelEvent = Nothing <$ R.ffilter isNothing textMayEvent
      textEvent = R.fmapMaybe id textMayEvent
      (failEvent, timeEvent) = R.fanEither $ myParseTime <$> textEvent
  warning <- R.holdDyn "" $ toText <$> failEvent
  D.elClass "span" "warning" $ D.dynText warning
  pure $
    R.leftmost
      [ Just . (\t -> time{zonedTimeToLocalTime = t}) <$> timeEvent
      , cancelEvent
      ]

dateSelectionWidget ::
  forall t m r e.
  StandardWidget t m r e =>
  Text ->
  Maybe UTCTime ->
  m (R.Event t (Maybe UTCTime))
dateSelectionWidget label utcTime = do
  timeZone <- zonedTimeZone <$> (R.sample . R.current =<< getTime)
  let timeDyn = utcToZonedTime timeZone <$> utcTime
  timeEvent <-
    fmap (Just . zonedTimeToUTC)
      <$> stateWidget False (selectTimeWidget label timeDyn)
  deleteEvent <- deleteTime $ isJust utcTime
  pure $ R.leftmost [deleteEvent, timeEvent]
 where
  deleteTime :: Bool -> m (R.Event t (Maybe a))
  deleteTime False = pure R.never
  deleteTime True = do
    event <- button "edit" $ icon "" "delete"
    pure $ Nothing <$ event

selectTimeWidget ::
  (StandardWidget t m r e) =>
  Text ->
  Maybe ZonedTime ->
  Bool ->
  m (R.Event t ZonedTime, R.Event t Bool)
selectTimeWidget label time True = do
  D.elClass "span" "" $ D.text (label <> ": ")
  currentTimeDyn <- getTime
  currentTime <- R.sample . R.current $ currentTimeDyn
  editEvent <- inputDateWidget $ fromMaybe currentTime time
  pure (R.fmapMaybe id editEvent, False <$ editEvent)
selectTimeWidget label time False = do
  editEvent <- showTime label time
  pure (R.never, True <$ editEvent)

showTime ::
  forall t m. Widget t m => Text -> Maybe ZonedTime -> m (R.Event t ())
showTime label = maybe create showWithButton
 where
  showWithButton time = do
    D.el "span" $ D.text label
    D.text . myFormatTime $ time
    button "edit" $ icon "" "edit"
  create = button "edit" $ do
    icon "" "add"
    D.elClass "span" "edit" $ D.text label
