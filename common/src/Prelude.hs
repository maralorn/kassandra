module Prelude
  ( module Relude
  , module Optics
  , module Optics.TH
  , module Data.Text.Optics
  , partitionEithersNE
  , Task
  , i
  , Aeson.ToJSON
  , Aeson.FromJSON
  , getZonedTime
  , addUTCTime
  , utcToZonedTime
  , zonedTimeZone
  , zonedTimeToUTC
  , firstJust
  , IOException
  , UTCTime
  , catch
  , concurrently_
  , forConcurrently_
  , withAsync
  , race_
  , makeLabels
  , Status
  , MonadFix
  , ZonedTime
  , NominalDiffTime
  , toJSON
  , fromJSON
  , UUID
  , HasField'
  , field'
  , HasAny
  , the
  , HasType
  , typed
  , AsType
  , _Typed
  , AsConstructor'
  , _Ctor'
  , formatTime
  , zonedTimeToLocalTime
  , LocalTime
  , defaultTimeLocale
  , parseTimeM
  )
where
import           Data.Time                      ( formatTime
                                                , defaultTimeLocale
                                                , parseTimeM
                                                , getZonedTime
                                                , utcToZonedTime
                                                , zonedTimeZone
                                                , zonedTimeToUTC
                                                , addUTCTime
                                                , UTCTime
                                                )
import           Data.Time.LocalTime            ( ZonedTime
                                                , LocalTime
                                                , zonedTimeToLocalTime
                                                )
import           Data.UUID                      ( UUID )
import           Control.Monad.Fix              ( MonadFix )
import           Relude                  hiding ( uncons )
import           Optics
import           Optics.TH
import           Data.Text.Optics        hiding ( text )
import           Taskwarrior.Task               ( Task )
import           Taskwarrior.Status             ( Status )
import           Data.These                     ( These(This, That, These) )
import qualified Data.Aeson                    as Aeson
import           Data.String.Interpolate        ( i )
import           Data.List.Extra                ( firstJust )
import           Control.Exception              ( IOException
                                                , catch
                                                )
import           Control.Concurrent.Async       ( concurrently_
                                                , forConcurrently_
                                                , withAsync
                                                , race_
                                                )
import           Language.Haskell.TH.Syntax     ( Q
                                                , Dec
                                                , Name
                                                )
import           Data.HashMap.Strict            ( HashMap )
import           Control.Monad.Reader           ( MonadReader )
import           Data.Time.Clock                ( NominalDiffTime )
import           Data.Aeson                     ( toJSON
                                                , fromJSON
                                                )
import           Data.Generics.Product.Fields   ( HasField'(field') )
import           Data.Generics.Product.Any      ( HasAny(the) )
import           Data.Generics.Product.Typed    ( HasType(typed) )
import           Data.Generics.Sum.Typed        ( AsType(_Typed) )
import           Data.Generics.Sum.Constructors ( AsConstructor'(_Ctor') )
import           GHC.Generics                   ( Generic )

-- | Stolen from these-1.0.1 which I can‘t import right now.
partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNE (x :| xs) = case (x, ls, rs) of
  (Left  y, ys    , []    ) -> This (y :| ys)
  (Left  y, ys    , z : zs) -> These (y :| ys) (z :| zs)
  (Right z, []    , zs    ) -> That (z :| zs)
  (Right z, y : ys, zs    ) -> These (y :| ys) (z :| zs)
  where (ls, rs) = partitionEithers xs

-- (lensField .~ noPrefixNamer $ fieldLabelsRules) == noPrefixFieldLabels but only in optics-th 0.2
makeLabels :: Name -> Q [Dec]
makeLabels = makeFieldLabelsWith noPrefixFieldLabels
