{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Frontend
  ( frontend
  )
where


import           Obelisk.Frontend
import           Obelisk.Route

import           Frontend.Css                   ( css )
import           Frontend.MainWidget            ( mainWidget )
import           Frontend.Types
import           Frontend.State                 ( StateProvider )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D

import           Common.Route                   ( FrontendRoute )

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = void $ D.prerender pass $ mainWidget webSocketStateProvider
  }

webSocketStateProvider :: WidgetIO t m => StateProvider t m
webSocketStateProvider _ = pure (R.constDyn mempty)

  --D.el "h1" $ (R.text "Welcome to my Obelisk!" >> R.dynText (show <$> timeDyn))
  --(R.domEvent R.Click . fst -> clickEvent) <- R.elAttr' "a" mempty
    -- $ R.text "Send all the tasks!"
  --socket <- R.jsonWebSocket
    --("ws://localhost:8000/socket" :: Text)
    --((lensVL R.webSocketConfig_send) .~ (one AllTasks <$ clickEvent) $ R.def)
  --answer :: R.Dynamic t (Maybe SocketMessage) <-
    --R.holdDyn Nothing $ socket ^. lensVL R.webSocket_recv
  --R.dynText (show <$> answer)

frontendHead :: ObeliskWidget js t route m => m ()
frontendHead = do
  D.el "title" $ D.text "Kassandra 2 Webversion"
  D.elAttr "style" mempty $ D.text css
