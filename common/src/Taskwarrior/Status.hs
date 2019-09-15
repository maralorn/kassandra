{-# LANGUAGE LambdaCase #-}
module Taskwarrior.Status
  ( Status(..)
  , parseStatus
  )
where

import           Taskwarrior.Mask               ( Mask )
import           Taskwarrior.Time               ( parseTime )
import           Data.Aeson                     ( Object
                                                , (.:)
                                                , Value(String)
                                                )
import           Control.Applicative            ( (<|>) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( UUID )
import           Data.Aeson.Types               ( typeMismatch
                                                , Parser
                                                )

data Status =
  Pending |
  Deleted { end :: UTCTime } |
  Completed { end :: UTCTime } |
  Waiting { wait :: UTCTime } |
  RecurringParent {
    recur :: Text,
    mask :: Mask} |
  RecurringChild {
    recur :: Text,
    imask :: Integer,
    parent :: UUID }
  deriving (Eq, Show)

type StatusParser = Object -> Parser Status

parseStatus, parseParent, parseChild :: StatusParser
parseStatus o = (o .: "status") >>= \case
  "pending"   -> pure Pending
  "deleted"   -> Deleted <$> (o .: "end" >>= parseTime)
  "completed" -> Completed <$> (o .: "end" >>= parseTime)
  "waiting"   -> Waiting <$> (o .: "wait" >>= parseTime)
  "recurring" -> parseParent o <|> parseChild o
  str         -> typeMismatch "status" (String str)

parseChild o =
  RecurringChild <$> o .: "recur" <*> o .: "imask" <*> o .: "parent"

parseParent o = RecurringParent <$> o .: "recur" <*> o .: "mask"
