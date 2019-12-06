{-# LANGUAGE TypeApplications, RecursiveDo, ScopedTypeVariables #-}
module TextEditWidget
  ( lineWidget
  , icon
  , button
  , dateSelectionWidget
  , createTextWidget
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
  let cancelEvent            = Nothing <$ R.ffilter isNothing textMayEvent
      textEvent              = R.fmapMaybe id textMayEvent
      (failEvent, timeEvent) = R.fanEither $ myParseTime <$> textEvent
  warning <- R.holdDyn "" $ pack <$> failEvent
  D.elClass "span" "warning" $ D.dynText warning
  pure $ R.leftmost
    [ Just . (\t -> time { zonedTimeToLocalTime = t }) <$> timeEvent
    , cancelEvent
    ]

stateWidget
  :: Widget t m
  => state
  -> (state -> m (R.Event t a, R.Event t state))
  -> m (R.Event t a)
stateWidget initialState widget = do
  rec state       <- R.holdDyn initialState stateEvent
      eventsEvent <- D.dyn $ widget <$> state
      stateEvent  <- R.switchHold R.never $ snd <$> eventsEvent
  R.switchHold R.never $ fst <$> eventsEvent

dateSelectionWidget
  :: forall t m r
   . StandardWidget t m r
  => Text
  -> R.Dynamic t (Maybe UTCTime)
  -> m (R.Event t (Maybe UTCTime))
dateSelectionWidget label utcTimeDyn = do
  currentTimeDyn <- getTime
  let timeDyn = do
        timeZone <- zonedTimeZone <$> currentTimeDyn
        fmap (utcToZonedTime timeZone) <$> utcTimeDyn
  timeEvent <- fmap (Just . zonedTimeToUTC)
    <$> stateWidget False (selectTimeWidget label timeDyn)
  deleteEventEvent <- D.dyn $ deleteTime <$> utcTimeDyn
  deleteEvent      <- R.switchHold R.never deleteEventEvent
  pure $ R.leftmost [deleteEvent, timeEvent]
 where
  deleteTime :: Maybe a -> m (R.Event t (Maybe a))
  deleteTime Nothing = pure R.never
  deleteTime Just{}  = do
    event <- button "edit" $ icon "" "delete"
    pure $ Nothing <$ event

selectTimeWidget
  :: (StandardWidget t m r)
  => Text
  -> R.Dynamic t (Maybe ZonedTime)
  -> Bool
  -> m (R.Event t ZonedTime, R.Event t Bool)
selectTimeWidget label timeDyn True = do
  D.elClass "span" "" $ D.text (label ++ ": ")
  time           <- R.sample . R.current $ timeDyn
  currentTimeDyn <- getTime
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
        D.el "span" $ D.text label
        D.text . myFormatTime $ time
        button "edit" $ icon "" "edit"
      create = do
        event <- button "edit" $ do
          icon "" "add"
          D.elClass "span" "edit" $ D.text label
        pure event
  eventEvent <- D.dyn $ maybe create showWithButton <$> timeDyn
  R.switchHold R.never eventEvent

lineWidget :: Widget t m => R.Dynamic t Text -> m (R.Event t Text)
lineWidget textDyn = enterTextWidget textDyn (showText textDyn)

createTextWidget :: Widget t m => m (R.Event t ()) -> m (R.Event t Text)
createTextWidget = enterTextWidget (R.constDyn "")

enterTextWidget
  :: Widget t m => R.Dynamic t Text -> m (R.Event t ()) -> m (R.Event t Text)
enterTextWidget textDyn altLabel =
  stateWidget False (selectWidget textDyn altLabel)

selectWidget
  :: Widget t m
  => R.Dynamic t Text
  -> m (R.Event t ())
  -> Bool
  -> m (R.Event t Text, R.Event t Bool)
selectWidget textDyn _ True = do
  textValue <- R.sample . R.current $ textDyn
  editEvent <- editText textValue
  pure (R.fmapMaybe id editEvent, False <$ editEvent)
selectWidget _ altLabel False = do
  editEvent <- altLabel
  pure (R.never, True <$ editEvent)


-- ! Takes a dynamic text and fires an event, when the user wants to edit it.
showText :: Widget t m => R.Dynamic t Text -> m (R.Event t ())
showText dynText = do
  D.dynText dynText
  button "edit" $ icon "" "edit"

-- ! Prompts the user for a text edit and fires an event, when the user confirms the result. Nothing is cancelation.
editText :: Widget t m => Text -> m (R.Event t (Maybe Text))
editText text = do
  textinput <-
    D.inputElement $ D.def D.& D.inputElementConfig_initialValue D..~ text
  saveButton <- button "" $ icon "" "save"
  let saveEvent = Just <$> R.tagPromptlyDyn
        (D._inputElement_value textinput)
        (D.keypress D.Enter textinput <> saveButton)
  cancelEvent <- button "" $ icon "" "cancel"
  pure $ R.leftmost [saveEvent, Nothing <$ cancelEvent]

icon :: Widget t m => Text -> Text -> m ()
icon cssClass = D.elClass "i" ("material-icons icon " ++ cssClass) . D.text

button :: Widget t m => Text -> m () -> m (R.Event t ())
button cssClass =
  fmap (D.domEvent D.Click . fst) . D.elClass' "span" ("button " ++ cssClass)
