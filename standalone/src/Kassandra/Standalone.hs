module Kassandra.Standalone
  ( standalone
  )
where

import qualified Reflex.Dom                    as D
import           Kassandra.MainWidget           ( mainWidget )
import           Kassandra.Standalone.State     ( ioStateProvider
                                                , ioStateFeeder
                                                )
import           Kassandra.Standalone.Config    ( readConfig
                                                , writeDeclarations
                                                )
import           Kassandra.Css                  ( css )
import           Kassandra.Debug                ( setLogLevel
                                                , Severity(..)
                                                , log
                                                )

standalone :: IO ()
standalone = do
  setLogLevel $ Just Debug
  log Info "Started kassandra"
  log Debug "Writing Types file"
  writeDeclarations
  --log Debug "Loading Config"
  --config <- readConfig Nothing
  --print config
  log Debug "Loaded Config"
  callbackSlot <- newEmptyMVar
  race_
    (ioStateFeeder callbackSlot)
    (D.mainWidgetWithCss (encodeUtf8 css) $ mainWidget $ ioStateProvider
      callbackSlot
    )


--type AppContext = (StateProvider, UIConfig)

--backendSelector :: [(Text, a)] -> m (R.Dynamic t (Text, a))
--standaloneWidget :: R.Dynamic t StandaloneAccount -> m (R.Dynamic t AppContext)
--localBackendWidget :: R.Dynamic t UserConfig -> m (R.Dynamic t AppContext)
--remoteBackendWidget :: R.Dynamic t RemoteBackend -> m (R.Dynamic t AppContext)
--mainWidget :: (StateProvider, UIConfig) -> m ()


--remoteBackend
--localBackend


