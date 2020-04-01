{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Frontend
  ( frontend
  )
where


import           Obelisk.Frontend
import           Obelisk.Route

import           Frontend.Css                   ( css )
import           Frontend.MainWidget            ( mainWidget )
import           Frontend.Types
import           Frontend.State                 ( StateProvider
                                                , stateProvider
                                                , CacheProvider
                                                , TaskProvider
                                                , Cache(Cache)
                                                )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import qualified Data.HashMap.Strict           as HashMap

import           Common.Route                   ( FrontendRoute )
import           Common.Api                     ( _ChangeTasks
                                                , _TaskUpdates
                                                )
import           Language.Javascript.JSaddle    ( MonadJSM )

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = void $ D.prerender pass $ mainWidget webSocketStateProvider
  }

type WidgetJSM t m
  = (D.HasJSContext m, MonadJSM (R.Performable m), MonadJSM m, WidgetIO t m)

webSocketStateProvider :: WidgetJSM t m => StateProvider t m
webSocketStateProvider = stateProvider tmpCacheProvider webSocketTaskProvider

webSocketTaskProvider :: forall t m . WidgetJSM t m => TaskProvider t m
webSocketTaskProvider changeTasksEvent = do
  socket <- D.jsonWebSocket
    ("ws://localhost:8000/socket" :: Text)
    (  (lensVL D.webSocketConfig_send)
    .~ (one . (_ChangeTasks #) <$> changeTasksEvent)
    $  D.def
    )
  let updateTasksEvents =
        R.fmapMaybe ((nonEmpty =<<) . (^? _TaskUpdates) =<<)
          $  socket
          ^. lensVL D.webSocket_recv
  R.foldDyn (flip . foldr . join $ HashMap.insert . (^. #uuid))
            HashMap.empty
            (changeTasksEvent <> updateTasksEvents)

tmpCacheProvider :: (WidgetIO t m) => CacheProvider t m
tmpCacheProvider toggleEvent = do
  fmap Cache
    <$> R.foldDyn (\values -> HashMap.union (fold $ one <$> values))
                  mempty
                  toggleEvent

  --D.el "h1" $ (R.text "Welcome to my Obelisk!" >> R.dynText (show <$> timeDyn))
  --(R.domEvent R.Click . fst -> clickEvent) <- R.elAttr' "a" mempty
    -- R.text "Send all the tasks!"
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
