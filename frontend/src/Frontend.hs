module Frontend
  ( frontend
  ) where


import           Obelisk.Frontend
import           Obelisk.Route

import           Frontend.Route                 ( FrontendRoute )
import           Kassandra.Config               ( NamedBackend(..)
                                                , PasswordConfig(..)
                                                , RemoteBackend(..)
                                                )
import           Kassandra.Css                  ( cssAsText )
import           Kassandra.MainWidget           ( mainWidget )
import           Kassandra.RemoteBackendWidget  ( remoteBackendWidget )
import           Kassandra.Types
import qualified Reflex.Dom                    as D

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend { _frontend_head = frontendHead
                    , _frontend_body = void $ D.prerender pass frontendBody
                    }

frontendBody :: WidgetJSM t m => m ()
frontendBody = do
  D.dyn_
    . (maybe pass
             (\(stateProvider, uiConfig) -> mainWidget uiConfig stateProvider) <$>
      )
    =<< remoteBackendWidget Nothing

frontendHead :: ObeliskWidget js t route m => m ()
frontendHead = do
  D.el "title" $ D.text "Kassandra 2 Webversion"
  D.elAttr "style" mempty $ D.text cssAsText
