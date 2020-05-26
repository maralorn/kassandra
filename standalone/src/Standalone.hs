{-# LANGUAGE PatternSynonyms #-}
module Standalone
  ( standalone
  )
where

import           Frontend.MainWidget            ( mainWidget )
import qualified Reflex.Dom                    as D
import           Standalone.State               ( ioStateProvider
                                                , ioStateFeeder
                                                )
import           Standalone.Config              ( readConfig )
import           Frontend.Css                   ( css )
import           Common.Debug (setLogLevel, pattern I, pattern D, log)

standalone :: IO ()
standalone = do
  setLogLevel $ Just D
  log I "Started kassandra"
  --config       <- readConfig Nothing
  callbackSlot <- newEmptyMVar
  race_
    (ioStateFeeder callbackSlot)
    (D.mainWidgetWithCss (encodeUtf8 css) $ mainWidget $ ioStateProvider
      callbackSlot
    )
