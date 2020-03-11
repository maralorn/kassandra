{-# LANGUAGE TypeApplications,  LambdaCase, RecursiveDo, ScopedTypeVariables, OverloadedStrings, OverloadedLabels, ViewPatterns #-}
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
  putStrLn "Started kassandra"
  D.mainWidgetWithCss (encodeUtf8 css) $ mainWidget ioStateProvider
