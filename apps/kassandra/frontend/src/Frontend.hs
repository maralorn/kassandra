module Frontend (
  frontend,
) where

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route

import Frontend.Route (FrontendRoute)
import Kassandra.Css (cssAsText)
import Kassandra.MainWidget (mainWidget)
import Kassandra.RemoteBackendWidget (
  CloseEvent (..),
  remoteBackendWidget,
 )
import Kassandra.Types
import qualified Reflex.Dom as D
import Relude.Extra.Newtype

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = frontendHead
    , _frontend_body = void $ D.prerender pass frontendBody
    }

frontendBody :: WidgetJSM t m => m ()
frontendBody = D.dyn_ . fmap (maybe pass mainWidget)
  =<< remoteBackendWidget (wrap D.never) Nothing

css = cssAsText (static @"MaterialIcons-Regular-Outlined.otf")

frontendHead :: ObeliskWidget js t route m => m ()
frontendHead = do
  D.el "title" $ D.text "Kassandra 2 Webversion"
  D.elAttr "style" mempty . D.text $ css
