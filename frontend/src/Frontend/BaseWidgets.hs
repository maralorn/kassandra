{-# LANGUAGE TypeApplications, RecursiveDo, ScopedTypeVariables, ViewPatterns, OverloadedLabels #-}
module Frontend.BaseWidgets
  ( icon
  , button
  , stateWidget
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Frontend.Types                 ( Widget )


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

icon :: Widget t m => Text -> Text -> m ()
icon cssClass = D.elClass "i" ("material-icons icon " <> cssClass) . D.text

button :: Widget t m => Text -> m () -> m (R.Event t ())
button cssClass =
  fmap (D.domEvent D.Click . fst) . D.elClass' "span" ("button " <> cssClass)
