{-# LANGUAGE PatternSynonyms #-}
module Standalone
  ( standalone
  )
where

import           Frontend.MainWidget            ( mainWidget )
import qualified Reflex.Dom                    as D

standalone :: IO ()
standalone = D.mainWidget mainWidget
