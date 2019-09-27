module Taskwarrior.Time
  ( parse
  , toValue
  )
where
import           Data.Aeson                     ( withText
                                                , Object
                                                )
import qualified Data.Aeson                    as Aeson
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

toValue :: UTCTime -> Aeson.Value
toValue = undefined

parse :: Aeson.Value -> Parser UTCTime
parse value = withText
  "Date"
  ( maybe (typeMismatch "Date" value) pure
  . parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%SZ"
  . unpack
  )
  value
