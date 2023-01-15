module Kassandra.SelectorWidget (
  backendSelector,
) where

import Kassandra.Config (NamedBackend (..))
import Kassandra.Types (Widget)
import qualified Reflex as R
import qualified Reflex.Dom as D

backendSelector ::
  Widget t m => NonEmpty (NamedBackend a) -> m (R.Dynamic t (NamedBackend a))
backendSelector backends = D.el "div" $ do
  buttons <- forM backends $ \backend -> do
    fmap ((backend <$) . D.domEvent D.Click . fst)
      . D.elClass' "a" "selector"
      . D.text
      $ name backend
  R.holdDyn (head backends) $ R.leftmost (toList buttons)
