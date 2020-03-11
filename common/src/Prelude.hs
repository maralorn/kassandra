{-# LANGUAGE TemplateHaskell, OverloadedLabels #-}
module Prelude
  ( module Relude
  , module Optics
  , module Optics.TH
  , module Data.Text.Optics
  , partitionEithersNE
  , UTCTime
  , Task.Task
  , i
  , Aeson.ToJSON
  , Aeson.FromJSON
  , getZonedTime
  , utcToZonedTime
  , zonedTimeZone
  , zonedTimeToUTC
  , UUID
  , MonadFix
  , firstJust
  , forkIO
  , IOException
  , catch
  )
where
import           Data.UUID                      ( UUID )
import           Data.Time                      ( UTCTime
                                                , getZonedTime
                                                , utcToZonedTime
                                                , zonedTimeZone
                                                , zonedTimeToUTC
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Relude                  hiding ( uncons )
import           Optics
import           Optics.TH
import           Data.Text.Optics        hiding ( text )
import qualified Taskwarrior.Task              as Task
import qualified Taskwarrior.Status            as Status
import           Data.These                     ( These(This, That, These) )
import qualified Data.Aeson                    as Aeson
import           Data.String.Interpolate        ( i )
import           Data.List.Extra                ( firstJust )
import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( IOException
                                                , catch
                                                )

-- | Stolen from these-1.0.1 which I can‘t import right now.
partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNE (x :| xs) = case (x, ls, rs) of
  (Left  y, ys    , []    ) -> This (y :| ys)
  (Left  y, ys    , z : zs) -> These (y :| ys) (z :| zs)
  (Right z, []    , zs    ) -> That (z :| zs)
  (Right z, y : ys, zs    ) -> These (y :| ys) (z :| zs)
  where (ls, rs) = partitionEithers xs

-- (lensField .~ noPrefixNamer $ fieldLabelsRules) == noPrefixFieldLabels but only in optics-th 0.2
makeFieldLabelsWith (lensField .~ const (const (one . TopName)) $ fieldLabelsRules) ''Task.Task
makeFieldLabelsWith (lensField .~ const (const (one . TopName)) $ fieldLabelsRules) ''Status.Status
makePrismLabels ''Status.Status
makePrismLabels ''Aeson.Result
