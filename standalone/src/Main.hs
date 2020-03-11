{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Frontend.MainWidget            ( mainWidget )
import qualified Reflex.Dom                    as D
import           State                          ( ioStateProvider )
import           Frontend.Css                   ( css )

main :: IO ()
main = do
  putTextLn "Started kassandra"
  D.mainWidgetWithCss (encodeUtf8 css) $ mainWidget ioStateProvider
