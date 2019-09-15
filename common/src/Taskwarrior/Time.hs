module Taskwarrior.Time
  ( maybeTime
  , parseTime
  )
where
import           Data.Aeson                     ( Value
                                                , withText
                                                , Object
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                )
import           Data.Time                      ( UTCTime
                                                , parseTimeM
                                                , defaultTimeLocale
                                                )
import           Data.Text                      ( unpack
                                                , Text
                                                )

maybeTime :: Text -> Object -> Parser (Maybe UTCTime)
maybeTime name o = traverse parseTime (HashMap.lookup name o)

parseTime :: Value -> Parser UTCTime
parseTime val = withText
  "Date"
  ( maybe (typeMismatch "Date" val) pure
  . parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%SZ"
  . unpack
  )
  val
