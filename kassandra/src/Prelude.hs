{-# OPTIONS_GHC -Wno-deprecations #-}
module Prelude (
  module Relude,
  module Optics,
  module Optics.TH,
  module Data.Text.Optics,
  partitionEithersNE,
  Task,
  i,
  (<$?>),
  (<&?>),
  Aeson.ToJSON,
  Aeson.FromJSON,
  getZonedTime,
  addUTCTime,
  utcToZonedTime,
  zonedTimeZone,
  zonedTimeToUTC,
  firstJust,
  IOException,
  UTCTime,
  catch,
  concurrently_,
  forConcurrently_,
  withAsync,
  race_,
  makeLabels,
  Status,
  MonadFix,
  ZonedTime,
  NominalDiffTime,
  toJSON,
  fromJSON,
  UUID,
  HasField',
  field',
  HasAny,
  the,
  HasType,
  typed,
  AsType,
  _Typed,
  AsConstructor',
  _Ctor',
  formatTime,
  zonedTimeToLocalTime,
  LocalTime,
  defaultTimeLocale,
  parseTimeM,
) where

import Control.Concurrent.Async (
  concurrently_,
  forConcurrently_,
  race_,
  withAsync,
 )
import Control.Exception (
  IOException,
  catch,
 )
import Control.Monad.Fix (MonadFix)
import Data.Aeson (
  fromJSON,
  toJSON,
 )
import qualified Data.Aeson as Aeson
import Data.Generics.Product.Any (HasAny (the))
import Data.Generics.Product.Fields (HasField' (field'))
import Data.Generics.Product.Typed (HasType (typed))
import Data.Generics.Sum.Constructors (AsConstructor' (_Ctor'))
import Data.Generics.Sum.Typed (AsType (_Typed))
import Data.List.Extra (firstJust)
import Data.String.Interpolate (i)
import Data.Text.Optics hiding (text)
import Data.These (partitionEithersNE)
import Data.Time (
  UTCTime,
  addUTCTime,
  defaultTimeLocale,
  formatTime,
  getZonedTime,
  parseTimeM,
  utcToZonedTime,
  zonedTimeToUTC,
  zonedTimeZone,
 )
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (
  LocalTime,
  ZonedTime,
  zonedTimeToLocalTime,
 )
import Data.UUID (UUID)
import Language.Haskell.TH.Syntax (
  Dec,
  Name,
  Q,
 )
import Optics
import Optics.TH
import Relude hiding (uncons)
import Taskwarrior.Status (Status)
import Taskwarrior.Task (Task)
import Data.Witherable ((<$?>), (<&?>))

-- (lensField .~ noPrefixNamer $ fieldLabelsRules) == noPrefixFieldLabels but only in optics-th 0.2
makeLabels :: Name -> Q [Dec]
makeLabels = makeFieldLabelsWith noPrefixFieldLabels
