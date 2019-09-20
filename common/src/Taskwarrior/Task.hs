module Taskwarrior.Task
  ( Task(..)
  , getTasks
  )
where

import           Data.Text                      ( Text
                                                , splitOn
                                                , unpack
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( UUID )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser )
import           Data.Aeson                     ( withObject
                                                , withText
                                                , FromJSON
                                                , parseJSON
                                                , (.:)
                                                , (.:?)
                                                , Value
                                                )

import           Taskwarrior.Status             ( Status
                                                , parseStatus
                                                )
import           Taskwarrior.Priority           ( Priority
                                                , maybePriority
                                                )
import           Taskwarrior.UDA                ( UDA )
import           Taskwarrior.Annotation         ( Annotation )
import           Taskwarrior.Time               ( maybeTime
                                                , parseTime
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           System.Process                 ( readProcess )


type Tag = Text

data Task = Task {
        status      :: Status,
        uuid        :: UUID,
        entry       :: UTCTime,
        description :: Text,
        start       :: Maybe UTCTime,
        modified    :: Maybe UTCTime,
        due         :: Maybe UTCTime,
        until       :: Maybe UTCTime,
        annotations :: [Annotation],
        scheduled   :: Maybe UTCTime,
        project     :: Maybe Text,
        priority    :: Maybe Priority,
        depends     :: [UUID],
        tags        :: [Tag],
        uda         :: UDA
} deriving (Eq, Show)

reservedKeys :: [Text]
reservedKeys =
  [ "status"
  , "uuid"
  , "description"
  , "entry"
  , "modified"
  , "due"
  , "until"
  , "scheduled"
  , "annotations"
  , "project"
  , "priority"
  , "depends"
  , "tags"
  , "wait"
  , "end"
  , "mask"
  , "imask"
  , "parent"
  ]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \o -> do
    status      <- parseStatus o
    uuid        <- o .: "uuid"
    entry       <- o .: "entry" >>= parseTime
    description <- o .: "description"
    start       <- maybeTime "start" o
    modified    <- maybeTime "modified" o
    due         <- maybeTime "due" o
    until_      <- maybeTime "until" o
    scheduled   <- maybeTime "scheduled" o
    annotations <- fmap (maybe [] id) $ o .:? "annotations"
    project     <- o .:? "project"
    priority    <- maybePriority o
    depends     <- maybe (pure []) parseUUIDList (HashMap.lookup "depends" o)
    tags        <- fmap (maybe [] id) $ o .:? "tags"
    uda <- pure $ HashMap.filterWithKey (\k _ -> k `notElem` reservedKeys) o
    pure Task { until = until_, .. }

parseUUIDList :: Value -> Parser [UUID]
parseUUIDList = withText "Text" $ mapM (parseJSON . Aeson.String) . splitOn ","

getTasks :: [Text] -> IO [Task]
getTasks args =
  readProcess "task" (map unpack . (++ ["export"]) $ args) ""
    >>= either fail return
    .   Aeson.eitherDecodeStrict
    .   encodeUtf8
    .   pack
