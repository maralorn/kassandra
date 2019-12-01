{-# LANGUAGE TypeApplications, TupleSections, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables #-}
module Main
  ( main
  )
where

import           GHC.Generics                   ( Generic )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.Aeson                    as Aeson
import qualified Data.Maybe                    as Maybe
import qualified Taskwarrior.Task              as Task
import qualified Taskwarrior.Status            as Status
import qualified Data.HashMap.Strict           as HashMap
import           Control.Lens.Getter            ( (^.) )
import           Data.HashMap.Strict            ( HashMap )
import           Data.UUID                      ( UUID )
import           Data.Time                      ( UTCTime )
import           Data.Time.LocalTime            ( getZonedTime
                                                , zonedTimeToUTC
                                                , zonedTimeZone
                                                , utcToZonedTime
                                                )
import           Types
import           ListWidget
import           State
import           Util
import           ClassyPrelude
import           Css

data Query = And [Query] | Or [Query] | Not Query | HasTag Text | DescriptionContains Text | Pending | CompletedAfter UTCTime | Deleted | CloseUpwards Query | Waiting | CloseDownwards Query deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

filterTasks :: HashMap UUID TaskInfos -> Query -> HashMap UUID TaskInfos
filterTasks tasks = \case
  And queries -> foldr (HashMap.intersection . filterTasks tasks) tasks queries
  Or     queries -> HashMap.unions . fmap (filterTasks tasks) $ queries
  Not    query   -> HashMap.difference tasks (filterTasks tasks query)
  HasTag tag     -> HashMap.filter (elem tag . Task.tags . task) tasks
  DescriptionContains text ->
    HashMap.filter (isInfixOf text . Task.description . task) tasks
  Pending -> HashMap.filter ((Status.Pending ==) . Task.status . task) tasks
  CompletedAfter end -> HashMap.filter (f . Task.status . task) tasks
   where
    f = \case
      Status.Completed { end = e } -> e >= end
      _                            -> False
  Deleted -> HashMap.filter (f . Task.status . task) tasks
   where
    f = \case
      Status.Deleted{} -> True
      _                -> False
  Waiting -> HashMap.filter (f . Task.status . task) tasks
   where
    f = \case
      Status.Waiting{} -> True
      _                -> False
  CloseUpwards query -> HashMap.foldr (HashMap.union . addParents)
                                      HashMap.empty
                                      (filterTasks tasks query)
   where
    addParents :: TaskInfos -> HashMap UUID TaskInfos
    addParents taskinfo =
      HashMap.singleton (Task.uuid $ task taskinfo) taskinfo
        `HashMap.union` maybe
                          HashMap.empty
                          addParents
                          (partof (task taskinfo) >>= (`HashMap.lookup` tasks))
  CloseDownwards query -> HashMap.foldr (HashMap.union . addChilds)
                                        HashMap.empty
                                        (filterTasks tasks query)
   where
    addChilds :: TaskInfos -> HashMap UUID TaskInfos
    addChilds taskinfo = HashMap.unions
      ( HashMap.singleton (Task.uuid $ task taskinfo) taskinfo
      : fmap
          addChilds
          (Maybe.mapMaybe (`HashMap.lookup` tasks) $ children taskinfo)
      )


main :: IO ()
main = do
  putStrLn "Started kassandra"
  D.mainWidgetWithCss (encodeUtf8 $ toStrict css) $ do
    time    <- liftIO $ getZonedTime
    timeDyn <-
      fmap (utcToZonedTime (zonedTimeZone time) . (^. R.tickInfo_lastUTC))
        <$> R.clockLossy 1 (zonedTimeToUTC time)
    D.text "Welcome to kassandra!"
    rec taskState         <- stateProvider stateChanges
        (_, stateChanges) <- R.runEventWriterT
          $ runReaderT widgetSwitcher (AppStateDyns taskState timeDyn)
    pure ()

widgets :: (ViewWidget t m (NonEmpty StateChange)) => [(Text, m ())]
widgets = [("Lists", listsWidget)]

widgetSwitcher :: forall t m . (ViewWidget t m (NonEmpty StateChange)) => m ()
widgetSwitcher = do
  buttons  <- mapM (\l -> (l <$) <$> D.button (fst l)) $ widgets @t @m
  listName <- R.holdDyn ("No list", pure ()) (R.leftmost buttons)
  _        <- D.dyn (snd <$> listName)
  pure ()
