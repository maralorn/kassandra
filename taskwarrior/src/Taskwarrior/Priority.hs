module Taskwarrior.Priority (parseMay, Priority) where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( Value, Object )
import           Data.Aeson.Types               ( Parser, typeMismatch )
import qualified Data.HashMap.Strict           as HashMap
import Control.Monad (join)

data Priority = High | Medium | Low
        deriving (Eq, Show, Read)


parseMay :: Value -> Parser (Maybe Priority)
parseMay val = Aeson.withText "Priority" (\case
  "H" -> pure $ Just High
  "M" -> pure $ Just Medium
  "L" -> pure $ Just Low
  "" -> pure Nothing
  _ -> typeMismatch "Priority" val) val
