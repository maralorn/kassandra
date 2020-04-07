{-# LANGUAGE TypeApplications, RecursiveDo, ScopedTypeVariables, ViewPatterns, OverloadedLabels #-}
module Frontend.TextEditWidget
  ( lineWidget
  , icon
  , button
  , createTextWidget
  , editText
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Frontend.Types


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

lineWidget :: Widget t m => Text -> m (R.Event t Text)
lineWidget text = enterTextWidget text (showText text)

createTextWidget :: Widget t m => m (R.Event t ()) -> m (R.Event t Text)
createTextWidget = enterTextWidget ""

enterTextWidget
  :: Widget t m => Text -> m (R.Event t ()) -> m (R.Event t Text)
enterTextWidget text altLabel =
  stateWidget False (selectWidget text altLabel)

selectWidget
  :: Widget t m
  => Text
  -> m (R.Event t ())
  -> Bool
  -> m (R.Event t Text, R.Event t Bool)
selectWidget text _ True = do
  editEvent <- editText text
  pure (R.fmapMaybe id editEvent, False <$ editEvent)
selectWidget _ altLabel False = do
  editEvent <- altLabel
  pure (R.never, True <$ editEvent)


-- ! Takes a dynamic text and fires an event, when the user wants to edit it.
showText :: Widget t m => Text -> m (R.Event t ())
showText text = do
  D.text text
  button "edit slimButton" $ icon "" "edit"

-- ! Prompts the user for a text edit and fires an event, when the user confirms the result. Nothing is cancelation.
editText :: Widget t m => Text -> m (R.Event t (Maybe Text))
editText text = D.elClass "span" "activeEdit" $ do
  textinput <-
    D.inputElement $ D.def & lensVL D.inputElementConfig_initialValue .~ text
  saveButton <- button "" $ icon "" "save"
  let saveEvent = Just <$> R.tag
        (textinput ^. to D._inputElement_value % #current)
        (D.keypress D.Enter textinput <> saveButton)
  cancelEvent <- button "" $ icon "" "cancel"
  pure $ R.leftmost [saveEvent, Nothing <$ cancelEvent]

icon :: Widget t m => Text -> Text -> m ()
icon cssClass = D.elClass "i" ("material-icons icon " <> cssClass) . D.text

button :: Widget t m => Text -> m () -> m (R.Event t ())
button cssClass =
  fmap (D.domEvent D.Click . fst) . D.elClass' "span" ("button " <> cssClass)
