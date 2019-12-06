module Util
  ( partof
  )
where
import           Taskwarrior.Task               ( Task )
import qualified Taskwarrior.Task              as Task
import qualified Data.Aeson                    as Aeson
import           ClassyPrelude
import           Data.UUID                      ( UUID )
import qualified Data.HashMap.Strict           as HashMap

partof :: Task -> Maybe UUID
partof t = do
  p <- HashMap.lookup "partof" $ Task.uda t
  case Aeson.fromJSON p of
    Aeson.Success a -> Just a
    _               -> Nothing
