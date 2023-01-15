{-# LANGUAGE PatternSynonyms #-}

module Kassandra.MainWidget (
  mainWidget,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import Kassandra.AgendaWidget (agendaWidget)
import Kassandra.BaseWidgets (button, br)
import Kassandra.Calendar (CalendarEvent)
import Kassandra.Config (DefinitionElement, Widget (DefinitionElementWidget, SearchWidget))
import Kassandra.Debug (
  Severity (..),
  log,
  logR,
  setLogLevel,
 )
import Kassandra.ListElementWidget (AdhocContext (NoContext), definitionElementWidget)
import Kassandra.ListWidget (listsWidget)
import Kassandra.LogWidget (logWidget)
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
  getTime,
 )
import Kassandra.Util (lookupTasks, stillTodo, tellNewTask)
import qualified Reflex as R
import qualified Reflex.Dom as D

mainWidget :: WidgetIO t m => StateProvider t m -> m ()
mainWidget stateProvider = do
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
      uiConfig <- R.holdUniqDyn $ appData ^. mapping #uiConfig
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
                uiConfig
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
            countTasks a = length . filter (has (#task % #status % a))
            pending = countTasks #_Pending taskList
            completed = countTasks #_Completed taskList
         in [i|#{pending} pending and #{completed} completed tasks. Kassandra-ToDo-Management|]

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

widgets :: StandardWidget t m r e => Seq (Text, m ())
widgets =
  fromList
    [ ("Agenda", agendaWidget)
    , ("Sort", nextWidget)
    , ("Tag Lists", listsWidget)
    , ("Logs", logWidget)
    ]

widgetSwitcher :: forall t m r e. StandardWidget t m r e => m ()
widgetSwitcher = do
  uiConfigD <- getAppState ^. mapping #uiConfig
  D.el "div" . D.dyn_ $ uiConfigD <&> withUIConfig
 where
  withUIConfig uiConfig = do
    let userWidgets = mkWidget <$> uiConfig ^. #viewList
    buttons <- forM (widgets <> userWidgets) selectButton
    listName <- R.holdDyn (fromMaybe ("No list", pass) (Seq.lookup 0 widgets)) (R.leftmost (toList buttons))
    D.el "div" $ D.dyn_ (snd <$> listName)
  selectButton label =
    (label <$) . D.domEvent D.Click . fst
      <$> D.elClass'
        "a"
        "selector"
        (D.text $ fst label)
  mkWidget :: Widget -> (Text, m ())
  mkWidget SearchWidget = ("Search", D.text "Not implemented")
  mkWidget (DefinitionElementWidget name definitionElement) = (name, definitionElementWidget NoContext definitionElement)

filterInbox :: UTCTime -> TaskState -> Seq CalendarEvent -> Seq TaskInfos
filterInbox now tasks events =
  Seq.reverse . Seq.sortOn (^. #modified) . fromList . toListOf (folded % filtered inInbox) $ tasks
 where
  scheduledEvents :: Set UUID
  scheduledEvents = fromList $ toList $ mapMaybe (^? #_ListElement % #_TaskwarriorTask) <$> (^. #todoList % #entries) =<< events
  inInbox :: TaskInfos -> Bool
  inInbox taskInfos =
    has (#tags % _Empty) taskInfos
      && maybe True (now >=) (taskInfos ^. #wait)
      && has (#status % #_Pending) taskInfos
      && (not . any stillTodo . lookupTasks tasks) (taskInfos ^. #children)
      && ( not
            . any (`notElem` ["kategorie", "root"])
            . Set.unions
            . view (mapping #tags)
            $ lookupTasks tasks (taskInfos ^. #parents)
         )
      && not (taskInfos ^. #blocked)
      && not ((taskInfos ^. #uuid) `Set.member` scheduledEvents)

getInboxTasks :: StandardWidget t m r e => m (D.Dynamic t (Seq TaskInfos))
getInboxTasks = do
  appState <- getAppState
  timeDyn <- zonedTimeToUTC <<$>> getTime
  let calendarEvents = appState ^. #calendarEvents
  tasks <- getTasks
  R.holdUniqDyn $ filterInbox <$> timeDyn <*> tasks <*> calendarEvents

nextWidget :: StandardWidget t m r e => m ()
nextWidget = do
  inboxTasks <- getInboxTasks
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
    (\x y -> if length x + length y > 0 then [i|There are #{length x} tasks in the inbox and #{length y} tasks unsorted.|] else "Nothing to do.")
      <$> inboxTasks <*> unsortedTasks
  inboxTaskDyn <- R.maybeDyn $ Seq.lookup 0 <$> inboxTasks
  let decorateSortTask x = D.el "p" (D.text "Sort this task into the task tree:") *> taskTreeWidget x
      decorateInboxTask x = D.el "p" $ do
         D.text "Process this task from the inbox:" *> br
         D.text "1. Does it need to be done?" *> br
         D.text "2. Can you do it in under 2 minutes?" *> br
         D.text "3. Should someone else do this?" *> br
         D.text "4. Should you split this task into sub tasks?" *> br
         D.text "5. On which tag list does it belong or when do you want to do it?" *> br
       *> taskTreeWidget x
      sortTask = do
        taskDyn <- R.maybeDyn $ viaNonEmpty head <$> unsortedTasks
        D.dyn_ (maybe pass decorateSortTask <$> taskDyn)
  D.dyn_ (maybe sortTask decorateInboxTask <$> inboxTaskDyn)
