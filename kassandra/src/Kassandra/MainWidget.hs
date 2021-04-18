{-# LANGUAGE PatternSynonyms #-}

module Kassandra.MainWidget (
  mainWidget,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import Kassandra.AgendaWidget (agendaWidget)
import Kassandra.BaseWidgets (button)
import Kassandra.Calendar (CalendarEvent)
import Kassandra.Config (DefinitionElement, UIConfig)
import Kassandra.Debug (
  Severity (..),
  log,
  logR,
  setLogLevel,
 )
import Kassandra.ListElementWidget (AdhocContext (NoContext), definitionElementWidget)
import Kassandra.ListWidget (listsWidget)
import Kassandra.LogWidget (logWidget)
import Kassandra.ReflexUtil (smartSimpleList)
import Kassandra.State (StateProvider)
import Kassandra.TaskWidget (taskTreeWidget)
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types (
  AppState (AppState),
  AppStateChange,
  StandardWidget,
  TaskInfos,
  TaskState,
  WidgetIO,
  getAppState,
  getSelectState,
  getTasks,
 )
import Kassandra.Util (lookupTasks, stillTodo, tellNewTask)
import qualified Reflex as R
import qualified Reflex.Dom as D

mainWidget :: WidgetIO t m => UIConfig -> StateProvider t m -> m ()
mainWidget _uiConfig stateProvider = do
  -- TODO: Use ui Config
  liftIO $ setLogLevel $ Just Info
  log Info "Loaded Mainwidget"
  time <- liftIO getZonedTime
  timeDyn <-
    fmap (utcToZonedTime (zonedTimeZone time) . (^. lensVL R.tickInfo_lastUTC))
      <$> R.clockLossy 1 (zonedTimeToUTC time)
  rec let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNESeq <$> stateChanges
      appData <- stateProvider dataChangeEvents
      selectedDyn <- R.holdDyn mempty $ NESeq.last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NESeq AppStateChange)) <-
        R.runEventWriterT $
          runReaderT
            ( do
                taskDiagnosticsWidget
                D.divClass "content" widgetSwitcher
                infoFooter
            )
            ( AppState
                (appData ^. mapping #taskState)
                timeDyn
                selectedDyn
                (appData ^. mapping #calendarData)
            )
      stateChanges <- logR Info (\a -> [i|StateChange: #{a}|]) stateChanges'
  pass

infoFooter :: StandardWidget t m r e => m ()
infoFooter = D.divClass "footer" $ do
  selectedState <- getSelectState
  D.dyn_ $
    selectedState <&> \a -> do
      whenJust (nonEmptySeq a) $ \(selectedTasks :: NESeq DefinitionElement) -> do
        forM_ selectedTasks $ \t -> D.divClass "selectedTask" (definitionElementWidget NoContext t)
  tellNewTask . fmap (,id)
    =<< createTextWidget
      (button "selector" $ D.text "New Task")
  tasks <- getTasks
  D.el "p" $
    D.dynText $
      tasks <&> \taskMap ->
        let taskList = HashMap.elems taskMap
            pending = length $ filter (has (#task % #status % #_Pending)) taskList
            waiting = length $ filter (has (#task % #status % #_Waiting)) taskList
            completed =
              length $ filter (has (#task % #status % #_Completed)) taskList
         in [i|#{pending} pending, #{waiting} waiting and #{completed} completed tasks. Kassandra-ToDo-Management|]

taskDiagnosticsWidget :: StandardWidget t m r e => m ()
taskDiagnosticsWidget = do
  tasks <- getTasks
  D.dynText $ do
    tasksMap <- tasks
    let uuids = HashMap.keys tasksMap
        hasLoop :: Seq UUID -> UUID -> Maybe UUID
        hasLoop seen new
          | new `elem` seen = Just new
          | otherwise = Seq.lookup 0 (mapMaybe (hasLoop (new <| seen)) nexts)
         where
          nexts = maybe mempty (^. #children) $ HashMap.lookup new tasksMap
    pure $
      firstJust (hasLoop mempty) uuids & \case
        Just uuid -> "Found a loop for uuid " <> show uuid
        Nothing -> "" -- everything fine

widgets :: StandardWidget t m r e => [(Text, m ())]
widgets =
  [ ("Next", nextWidget)
  , ("Lists", listsWidget)
  , ("Inbox", inboxWidget)
  , ("Unsorted", unsortedWidget)
  , ("Agenda", agendaWidget)
  , ("Logs", logWidget)
  ]

widgetSwitcher :: forall t m r e. StandardWidget t m r e => m ()
widgetSwitcher = D.el "div" $ do
  buttons <- forM (widgets @t @m) $ \l ->
    (l <$) . D.domEvent D.Click . fst
      <$> D.elClass'
        "a"
        "selector"
        (D.text $ fst l)
  listName <- R.holdDyn ("No list", pass) (R.leftmost buttons)
  D.el "div" $ D.dyn_ (snd <$> listName)

filterInbox :: TaskState -> Seq CalendarEvent -> Seq TaskInfos
filterInbox tasks events =
  Seq.sortOn (^. #modified) . fromList . toListOf (folded % filtered inInbox) $ tasks
 where
  scheduledEvents :: Set UUID
  scheduledEvents = fromList $ toList $ mapMaybe (^? #_ListElement % #_TaskwarriorTask) <$> (^. #todoList % #entries) =<< events
  inInbox :: TaskInfos -> Bool
  inInbox taskInfos =
    has (#tags % _Empty) taskInfos
      && has (#status % #_Pending) taskInfos
      && (not . any stillTodo . lookupTasks tasks) (taskInfos ^. #children)
      && ( not
            . any (`notElem` ["kategorie", "project", "root"])
            . Set.unions
            . view (mapping #tags)
            $ lookupTasks tasks (taskInfos ^. #parents)
         )
      && not (taskInfos ^. #blocked)
      && not ((taskInfos ^. #uuid) `Set.member` scheduledEvents)

getInboxTasks :: StandardWidget t m r e => m (D.Dynamic t (Seq TaskInfos))
getInboxTasks = do
  appState <- getAppState
  let calendarEvents = appState ^. #calendarEvents
  tasks <- getTasks
  R.holdUniqDyn $ filterInbox <$> tasks <*> calendarEvents

nextWidget :: StandardWidget t m r e => m ()
nextWidget = do
  inboxTasks <- getInboxTasks
  D.dynText $
    (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
      <$> inboxTasks
  void . smartSimpleList (taskTreeWidget . pure) $ Seq.take 1 <$> inboxTasks

inboxWidget :: StandardWidget t m r e => m ()
inboxWidget = do
  inboxTasks <- getInboxTasks
  D.dynText $
    (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
      <$> inboxTasks
  void . smartSimpleList (taskTreeWidget . pure) $ inboxTasks

unsortedWidget :: StandardWidget t m r e => m ()
unsortedWidget = do
  unsortedTasks <-
    fmap
      ( filter
          ( \task ->
              not (Set.member "root" (task ^. #tags))
                && has (#partof % _Nothing) task
                && has (#status % #_Pending) task
          )
          . HashMap.elems
      )
      <$> getTasks
  D.dynText $
    (\x -> "There are " <> show (length x) <> " unsorted tasks.")
      <$> unsortedTasks
  void . flip R.simpleList taskTreeWidget $ unsortedTasks
