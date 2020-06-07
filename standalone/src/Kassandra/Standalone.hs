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
                                                , StandaloneConfig
                                                )
import           Kassandra.Css                  ( css )
import           Kassandra.Config.Dhall         ( dhallType )
import           Kassandra.Debug                ( setLogLevel
                                                , Severity(..)
                                                , log
                                                )

standalone :: IO ()
standalone = do
  setLogLevel $ Just Debug
  log Info "Started kassandra"
  putTextLn $ dhallType @StandaloneConfig
  --log Debug "Loading Config"
  --config <- readConfig Nothing
  --print config
  --log Debug "Loaded Config"
  callbackSlot <- newEmptyMVar
  race_
    (ioStateFeeder callbackSlot)
    (D.mainWidgetWithCss (encodeUtf8 css) $ mainWidget $ ioStateProvider
      callbackSlot
    )
