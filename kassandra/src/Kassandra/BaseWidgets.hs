module Kassandra.BaseWidgets (
  br,
  icon,
  button,
  stateWidget,
) where

import Kassandra.Types (Widget)
import qualified Reflex as R
import qualified Reflex.Dom as D
import Relude.Extra.Bifunctor (secondF)

br :: D.DomBuilder t m => m ()
br = D.el "br" pass

stateWidget ::
  Widget t m =>
  state ->
  (state -> m (R.Event t a, R.Event t state)) ->
  m (R.Event t a)
stateWidget initialState widget = do
  eventsEvent <- D.workflowView $ stateToWorkflow initialState
  R.switchHold R.never eventsEvent
 where
  stateToWorkflow = D.Workflow . secondF (fmap stateToWorkflow) . widget

icon :: Widget t m => Text -> Text -> m ()
icon cssClass = D.elClass "i" ("material-icons icon " <> cssClass) . D.text

button :: Widget t m => Text -> m () -> m (R.Event t ())
button cssClass =
  fmap (D.domEvent D.Click . fst) . D.elClass' "span" ("button " <> cssClass)
