module Kassandra.SelectorWidget
  ( backendSelector
  )
where

import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Kassandra.Types                ( Widget )
import           Kassandra.Config               ( name
                                                , NamedBackend
                                                )

backendSelector
  :: Widget t m => NonEmpty (NamedBackend a) -> m (R.Dynamic t (NamedBackend a))
backendSelector backends = D.el "div" $ do
  buttons <-
    (forM backends $ \backend -> do
      fmap ((backend <$) . D.domEvent D.Click . fst)
        . D.elClass' "a" "selector"
        . D.text
        $ name backend
    )
  R.holdDyn (head backends) $ R.leftmost (toList buttons)
