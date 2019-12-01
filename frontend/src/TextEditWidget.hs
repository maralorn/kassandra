{-# LANGUAGE TypeApplications, TupleSections, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables #-}
module TextEditWidget
  ( lineWidget
  , iconButton
  , iconButtonClass
  , icon
  , iconClass
  , dateSelectionWidget
  )
where
import           ClassyPrelude
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Types
import           Data.Time.LocalTime            ( ZonedTime
                                                , zonedTimeZone
                                                , utcToZonedTime
                                                , zonedTimeToUTC
                                                , LocalTime
                                                , zonedTimeToLocalTime
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )

myFormatTime :: ZonedTime -> Text
myFormatTime = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

myParseTime :: Text -> Either String LocalTime
myParseTime t =
  maybe (Left ("'" ++ text ++ "' cannot be parsed as '%Y-%m-%d %H:%M'.")) Right
    . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
    $ text
  where text = unpack t

inputDateWidget
  :: forall t m . Widget t m => ZonedTime -> m (R.Event t (Maybe ZonedTime))
inputDateWidget time = do
  textMayEvent <-
    editText @t @m . myFormatTime $ time :: m (R.Event t (Maybe Text))
  let cancelEvent            = Nothing <$ R.ffilter (isNothing) textMayEvent
      textEvent              = R.fmapMaybe id textMayEvent
      (failEvent, timeEvent) = R.fanEither $ myParseTime <$> textEvent
  warning <- R.holdDyn "" $ pack <$> failEvent
  D.elClass "span" "warning" $ D.dynText warning
  pure $ R.leftmost
    [ Just . (\t -> time { zonedTimeToLocalTime = t }) <$> timeEvent
    , cancelEvent
    ]

dateSelectionWidget
  :: ViewWidget t m (NonEmpty (Maybe UTCTime))
  => Text
  -> R.Dynamic t (Maybe UTCTime)
  -> m ()
dateSelectionWidget label utcTimeDyn = do
  currentTimeDyn <- getTime <$> ask
  let timeDyn = do
        timeZone <- zonedTimeZone <$> currentTimeDyn
        fmap (utcToZonedTime timeZone) <$> utcTimeDyn
  rec editMode    <- R.holdDyn False toggleEvent
      eventsEvent <- D.dyn $ selectTimeWidget label timeDyn <$> editMode
      toggleEvent <- R.switchHold R.never (snd <$> eventsEvent)
  timeEvent <- R.switchHold R.never (fst <$> eventsEvent)
  R.tellEvent $ singleton . Just . zonedTimeToUTC <$> timeEvent


selectTimeWidget
  :: (Widget t m, MonadReader (AppStateDyns t) m)
  => Text
  -> R.Dynamic t (Maybe ZonedTime)
  -> Bool
  -> m (R.Event t ZonedTime, R.Event t Bool)
selectTimeWidget label timeDyn True = do
  icon "schedule"
  D.elClass "span" "" $ D.text label
  time           <- R.sample . R.current $ timeDyn
  currentTimeDyn <- getTime <$> ask
  currentTime    <- R.sample . R.current $ currentTimeDyn
  editEvent      <- inputDateWidget $ fromMaybe currentTime time
  pure (R.fmapMaybe id editEvent, False <$ editEvent)
selectTimeWidget label timeDyn False = do
  editEvent <- showTime label timeDyn
  pure (R.never, True <$ editEvent)

showTime
  :: forall t m
   . Widget t m
  => Text
  -> R.Dynamic t (Maybe ZonedTime)
  -> m (R.Event t ())
showTime label timeDyn = do
  let showWithButton time = do
        icon @t @m "schedule"
        D.el "span" $ D.text label
        D.text . myFormatTime $ time
        iconButton "edit"
      create = do
        event <- iconButtonClass @t @m "schedule" "edit"
        D.elClass "span" "edit" $ D.text label
        pure event
  eventEvent <- D.dyn $ maybe create showWithButton <$> timeDyn
  R.switchHold R.never eventEvent

lineWidget :: Widget t m => R.Dynamic t Text -> m (R.Event t Text)
lineWidget textDyn = do
  rec editMode    <- R.holdDyn False toggleEvent
      eventsEvent <- D.dyn $ selectWidget textDyn <$> editMode
      textEvent   <- R.switchHold R.never (fst <$> eventsEvent)
      toggleEvent <- R.switchHold R.never (snd <$> eventsEvent)
  pure textEvent

selectWidget
  :: Widget t m
  => R.Dynamic t Text
  -> Bool
  -> m (R.Event t Text, R.Event t Bool)
selectWidget textDyn True = do
  textValue <- R.sample . R.current $ textDyn
  editEvent <- editText textValue
  pure (R.fmapMaybe id editEvent, False <$ editEvent)
selectWidget textDyn False = do
  editEvent <- showText textDyn
  pure (R.never, True <$ editEvent)


-- ! Takes a dynamic text and fires an event, when the user wants to edit it.
showText :: Widget t m => R.Dynamic t Text -> m (R.Event t ())
showText dynText = do
  D.dynText dynText
  iconButtonClass "edit" "edit"

-- ! Prompts the user for a text edit and fires an event, when the user confirms the result. Nothing is cancelation.
editText :: Widget t m => Text -> m (R.Event t (Maybe Text))
editText text = do
  textinput <-
    D.inputElement $ D.def D.& D.inputElementConfig_initialValue D..~ text
  saveButton <- iconButton "save"
  let saveEvent = Just <$> R.tagPromptlyDyn
        (D._inputElement_value textinput)
        (D.keypress D.Enter textinput <> saveButton)
  cancelEvent <- iconButton "cancel"
  pure $ R.leftmost [saveEvent, Nothing <$ cancelEvent]

icon :: Widget t m => Text -> m ()
icon = flip iconClass ""

iconClass :: Widget t m => Text -> Text -> m ()
iconClass label cssClass =
  D.elClass "i" ("material-icons " ++ cssClass) . D.text $ label

iconButtonClass :: Widget t m => Text -> Text -> m (R.Event t ())
iconButtonClass label cssClass = do
  (elt, _) <- D.elClass' "i" ("button material-icons " ++ cssClass)
    $ D.text label
  pure $ D.domEvent D.Click elt

iconButton :: Widget t m => Text -> m (R.Event t ())
iconButton = flip iconButtonClass ""
