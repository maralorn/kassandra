module Frontend.Route (
  BackendRoute (..),
  FrontendRoute (..),
  fullRouteEncoder,
) where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import qualified Control.Category
import Data.Text (Text)

--import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: Type -> Type where
  -- | Used to handle unparseable routes.
  BackendRouteMissing :: BackendRoute ()
  BackendRouteSocket :: BackendRoute PageName

-- You can define any routes that will be handled specially by the backend here.
-- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: Type -> Type where
  FrontendRouteMain :: FrontendRoute ()

-- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder ::
  Encoder
    (Either Text)
    Identity
    (R (FullRoute BackendRoute FrontendRoute))
    PageName
fullRouteEncoder =
  mkFullRouteEncoder
    (FullRoute_Backend BackendRouteMissing :/ ())
    ( \case
        BackendRouteMissing -> PathSegment "missing" $ unitEncoder mempty
        BackendRouteSocket -> PathSegment "socket" $ Control.Category.id
    )
    ( \case
        FrontendRouteMain -> PathEnd $ unitEncoder mempty
    )

concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute
    , ''FrontendRoute
    ]
