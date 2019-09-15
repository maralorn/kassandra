{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Taskwarrior.Annotation
  ( Annotation(..)
  )
where

import           Taskwarrior.Time               ( parseTime )
import           Data.Time                      ( UTCTime )
import           Data.Text                      ( Text )
import           Data.Aeson                     ( FromJSON
                                                , withObject
                                                , (.:)
                                                )
import qualified Data.Aeson                    as Aeson

data Annotation = Annotation { entry :: UTCTime, description :: Text } deriving (Eq, Show, Read, Ord)

instance FromJSON Annotation where
  parseJSON = withObject "Annotation" $ \o -> do
    description <- o .: "description"
    entry       <- o .: "entry" >>= parseTime
    pure Annotation { .. }
