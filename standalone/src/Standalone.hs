{-# LANGUAGE OverloadedStrings #-}
module Standalone
  ( standalone
  )
where

import           Frontend.MainWidget            ( mainWidget )
import qualified Reflex.Dom                    as D
import           State                          ( ioStateProvider
                                                , ioStateFeeder
                                                )
import           Frontend.Css                   ( css )

standalone :: IO ()
standalone = do
  putTextLn "Started kassandra"
  callbackSlot <- newEmptyMVar
  race_
    (ioStateFeeder callbackSlot)
    (D.mainWidgetWithCss (encodeUtf8 css) $ mainWidget $ ioStateProvider
      callbackSlot
    )
