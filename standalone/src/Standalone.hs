{-# LANGUAGE PatternSynonyms #-}
module Standalone
  ( standalone
  )
where

import           Frontend.MainWidget            ( mainWidget )
import Reflex.Host.Headless

standalone :: IO ()
standalone = runHeadlessApp mainWidget
