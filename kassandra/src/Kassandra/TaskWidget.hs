module Kassandra.TaskWidget (
  taskTreeWidget,
  taskList,
  uuidWidget,
) where

import qualified Data.HashSet as HashSet
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Kassandra.BaseWidgets (
  button,
  icon, br
 )
import Kassandra.Config (DefinitionElement)
import Kassandra.Debug (
  Severity (..),
  log,
 )
import Kassandra.DragAndDrop (
  childDropArea,
  taskDropArea,
  tellSelected,
 )
import Kassandra.ReflexUtil (listWithGaps)
import Kassandra.Sorting (
  SortMode (SortModePartof),
  SortPosition (SortPosition),
  sortTasks,
 )
import Kassandra.TextEditWidget (
  createTextWidget,
  lineWidget,
 )
import Kassandra.TimeWidgets (dateSelectionWidget)
import Kassandra.Types (
  AppState,
  Have,
  StandardWidget,
  TaskInfos,
  TaskTreeState,
  TaskTreeStateChange,
  TaskTreeWidget,
  ToggleEvent (ToggleEvent),
  getAppState,
  getExpandedTasks,
  getIsExpanded,
  getSelectState,
  getTime,
 )
import Kassandra.Util (lookupTaskM, lookupTasksDynM, lookupTasksM, stillTodo, tellNewTask, tellTask, tellToggle)
import qualified Reflex as R
import Reflex.Dom ((=:))
import qualified Reflex.Dom as D
import qualified Taskwarrior.Status as Status
import Taskwarrior.UDA (UDA)

type TaskWidget t m r e = (TaskTreeWidget t m r e, HaveTask m r)
type HaveTask m r = Have m r TaskInfos

instance LabelOptic "taskInfos" A_Lens (a, TaskInfos) (a, TaskInfos) TaskInfos TaskInfos where
  labelOptic = _2

getTaskInfos :: HaveTask m r => m TaskInfos
getTaskInfos = ask ^. mapping typed

getChildren :: TaskWidget t m r e => m (R.Dynamic t (Seq TaskInfos))
getChildren = getTaskInfos ^. mapping #children >>= lookupTasksM

taskTreeWidget ::
  forall t m r e. StandardWidget t m r e => R.Dynamic t TaskInfos -> m ()
taskTreeWidget taskInfosD = do
  log Debug "Creating Tasktree Widget"
  (appState :: AppState t) <- getAppState
  rec treeState <-
        R.foldDyn
          ( flip $
              foldr
                ( \case
                    ToggleEvent uuid False -> HashSet.delete uuid
                    ToggleEvent uuid True -> HashSet.insert uuid
                ) ::
              NESeq ToggleEvent -> HashSet UUID -> HashSet UUID
          )
          mempty
          treeStateChanges
      (_, events :: R.Event t (NESeq TaskTreeStateChange)) <-
        R.runEventWriterT $
          runReaderT (taskWidget taskInfosD) (appState, treeState)
      let (appStateChanges, treeStateChanges) =
            R.fanThese $ partitionEithersNESeq <$> events
  R.tellEvent (fmap (_Typed #) <$> appStateChanges)

taskWidget ::
  forall t m r e. (TaskTreeWidget t m r e) => R.Dynamic t TaskInfos -> m ()
taskWidget taskInfos' = D.divClass "task" $ do
  taskInfosD <- R.holdUniqDyn taskInfos'
  appState <- getAppState
  treeState <- ask ^. mapping typed
  D.dyn_ $ taskInfosD <&> \taskInfos -> runReaderT widgets (appState, taskInfos, treeState)
  childrenWidget taskInfosD
 where
  widgets :: ReaderT (AppState t, TaskInfos, TaskTreeState t) m ()
  widgets = D.divClass "uppertask" $ do
    D.divClass "statusWrapper" statusWidget
    D.divClass "righttask" $ do
      collapseButton
      descriptionWidget
      tagsWidget
      waitWidget
      dueWidget
      pathWidget
      parentButton
      dependenciesWidget
      addChildWidget
      deleteButton
      completedWidget
      selectWidget
      dropChildWidget

pathWidget :: TaskWidget t m r e => m ()
pathWidget = do
  parents <- getTaskInfos ^. mapping #parents >>= lookupTasksM ^. mapping (mapping (mapping (mapping #description)))
  D.dyn_ $ flip whenJust showPath . nonEmptySeq <$> parents
 where
  showPath :: TaskWidget t m r e => NESeq Text -> m ()
  showPath parents = D.elClass "span" "parentPath" $ do
    br
    makePath parents

makePath :: (TaskWidget t m r e) => NESeq Text -> m ()
makePath = D.elClass "span" "path" . D.text . Text.intercalate " ≫ " . toList . NESeq.reverse

dependenciesWidget :: (TaskWidget t m r e) => m ()
dependenciesWidget = do
  taskInfos <- getTaskInfos
  revDepends <- filter stillTodo <<$>> lookupTasksM (taskInfos ^. #revDepends)
  depends <- filter stillTodo <<$>> (lookupTasksM . toList) (taskInfos ^. #depends)
  D.dyn_ $
    whenNotNull <$> depends
      <*> pure
        ( \ds -> do
            br
            D.text "dependencies: "
            forM_ ds $ \d -> do
              makeOwnPath d
              deleteEvent <- button "edit" $ icon "" "delete"
              tellTask $ removeDep (taskInfos ^. #task) d <$ deleteEvent
              br
        )
  D.dyn_ $
    whenJust . nonEmptySeq <$> revDepends
      <*> pure
        ( \rds -> do
            br
            D.text "reverse dependencies: "
            forM_ rds $ \rd -> do
              makeOwnPath rd
              deleteEvent <- button "edit" $ icon "" "delete"
              tellTask $ removeDep (rd ^. #task) taskInfos <$ deleteEvent
              br
        )
 where
  removeDep ::
    ( Is k1 A_Getter
    , Is k2 A_Setter
    , LabelOptic "depends" k2 s1 t (Set UUID) (Set UUID)
    , LabelOptic "uuid" k1 s2 s2 UUID UUID
    ) =>
    s1 ->
    s2 ->
    t
  removeDep task dependency =
    #depends %~ Set.filter (dependency ^. #uuid /=) $ task

makeOwnPath :: (TaskWidget t m r e) => TaskInfos -> m ()
makeOwnPath task =
  D.dyn_
    . fmap (makePath . (\ps -> task ^. #description :<|| ps))
    =<< lookupTasksM (task ^. #parents) ^. mapping (mapping (mapping #description))

dropChildWidget :: (TaskWidget t m r e) => m ()
dropChildWidget = do
  taskInfos <- getTaskInfos
  childrenD <- getChildren
  showIcon <- fmap not <$> getIsExpanded (taskInfos ^. #uuid)
  D.dyn_ $
    when <$> showIcon
      <*> pure
        ( childDropArea
            ( SortPosition
                (SortModePartof <$> taskInfos ^. #uuid % to R.constant)
                (childrenD ^. mapping (mapping #task) % #current)
                (R.constant Nothing)
            )
            ( R.constDyn
                (taskInfos ^. #uuid <| taskInfos ^. #parents <> taskInfos ^. #children)
            )
            $ icon "dropHere" "move_to_inbox"
        )
  taskDropArea
    (taskInfos ^. #uuid % to (R.constDyn . one))
    (icon "dropHere plusOne" "block")
    $ fmap
      ( \dependencies ->
          one $
            #depends
              %~ Set.union (Set.fromList $ toList $ (^. #uuid) <$> dependencies)
              $ taskInfos
                ^. #task
      )
  taskDropArea
    (taskInfos ^. #uuid % to (R.constDyn . one))
    (icon "dropHere plusTwo" "schedule")
    $ fmap (fmap ((#depends %~ Set.insert (taskInfos ^. #uuid)) . (^. #task)))

tagsWidget :: forall t m r e. TaskWidget t m r e => m ()
tagsWidget = do
  task <- getTaskInfos ^. mapping #task
  forM_ (task ^. #tags) $ \tag -> D.elClass "span" "tag" $ do
    D.text tag
    deleteEvent <- button "edit" $ icon "" "delete"
    tellTask $ (#tags %~ Set.filter (tag /=) $ task) <$ deleteEvent
  tagEvent <- createTextWidget . button "edit" $ icon "" "add_box"
  tellTask $ (\tag -> #tags %~ Set.insert tag $ task) <$> tagEvent

getNewUDA :: forall t m r e. TaskWidget t m r e => m UDA
getNewUDA = one . ("partof",) . toJSON <$> getTaskInfos ^. mapping #uuid

addChildWidget :: TaskWidget t m r e => m ()
addChildWidget = do
  descriptionEvent <- createTextWidget . button "edit" $ icon "" "add_task"
  newUDA <- getNewUDA
  tellNewTask $ (,#uda .~ newUDA) <$> descriptionEvent

childrenWidget :: forall t m r e. TaskTreeWidget t m r e => R.Dynamic t TaskInfos -> m ()
childrenWidget taskInfosD = do
  expandedTasks <- getExpandedTasks
  showChildren <-
    R.holdUniqDyn $
      R.zipDynWith HashSet.member (taskInfosD ^. mapping #uuid) expandedTasks
  D.dyn_ $ showOptional <$> showChildren
 where
  showOptional :: Bool -> m ()
  showOptional x = when x $ do
    children <-
      R.holdUniqDyn . fmap (filter stillTodo) =<< lookupTasksDynM
        =<< R.holdUniqDyn
          (taskInfosD ^. mapping #children)
    let sortModeD = SortModePartof <$> taskInfosD ^. mapping #uuid
    blacklist <-
      R.holdUniqDyn $
        liftA2 (<|) (taskInfosD ^. mapping #uuid) (taskInfosD ^. mapping #parents)
    sortedList <- R.holdUniqDyn $ sortTasks <$> sortModeD <*> children
    D.divClass "children" $
      taskList (sortModeD ^. #current) sortedList blacklist taskWidget


taskList ::
  StandardWidget t m r e =>
  R.Behavior t SortMode ->
  R.Dynamic t (Seq TaskInfos) ->
  R.Dynamic t (Seq UUID) ->
  (R.Dynamic t TaskInfos -> m ()) ->
  m ()
taskList mode tasksD blacklistD elementWidget =
  listWithGaps (uuidWidget elementWidget . pure) gapWidget uuidsD
 where
  gapWidget pairD =
    childDropArea
      (partialSortPosition (snd <$> R.current pairD))
      ((maybesToSeq <$> pairD) <> blacklistD)
      $ icon "dropHere above" "forward"
  maybesToSeq (a, b) = fromList (toList a <> toList b)
  partialSortPosition = SortPosition mode (tasksD ^. mapping (mapping #task) % #current)
  uuidsD = tasksD ^. mapping (mapping #uuid)

uuidWidget :: StandardWidget t m r e => (R.Dynamic t TaskInfos -> m ()) -> R.Dynamic t UUID -> m ()
uuidWidget widget uuid = do
  maybeCurrentTaskD <- R.maybeDyn =<< R.holdUniqDyn =<< lookupTaskM uuid
  D.dyn_ $
    maybe
      (D.dynText $ (\u -> [i|Task #{u} not found.|]) <$> uuid)
      widget
      <$> maybeCurrentTaskD

waitWidget :: forall t m r e. TaskWidget t m r e => m ()
waitWidget = do
  task <- getTaskInfos ^. mapping #task
  event <- getTaskInfos >>= ((^. #wait) >>> dateSelectionWidget "wait")
  tellTask $ flip (#wait .~) task <$> event

dueWidget :: TaskWidget t m r e => m ()
dueWidget = do
  task <- getTaskInfos ^. mapping #task
  event <- dateSelectionWidget "due" $ task ^. #due
  tellTask $ flip (#due .~) task <$> event

selectWidget :: TaskWidget t m r e => m ()
selectWidget = do
  uuid <- getTaskInfos ^. mapping (#task % #uuid)
  (dragEl, _) <- D.elClass' "span" "button" $ icon "" "filter_list"
  selectStateB <- toggleContainUUID uuid <<$>> R.current <$> getSelectState
  tellSelected $ R.tag selectStateB (D.domEvent D.Click dragEl)
 where
  toggleContainUUID :: UUID -> Seq DefinitionElement -> Seq DefinitionElement
  toggleContainUUID ((#_ListElement % #_TaskwarriorTask #) -> entry) selectedTasks =
    Seq.findIndexL (== entry) selectedTasks & maybe (selectedTasks |> entry) (`Seq.deleteAt` selectedTasks)

descriptionWidget :: TaskWidget t m r e => m ()
descriptionWidget = do
  task <- getTaskInfos ^. mapping #task
  event <- lineWidget $ task ^. #description
  tellTask $ flip (#description .~) task <$> event

tellStatusByTime ::
  TaskWidget t m r e => ((UTCTime -> Status) -> R.Event t a -> m ())
tellStatusByTime handler ev = do
  time <- getTime
  tellStatus $ handler . zonedTimeToUTC <$> R.tag (R.current time) ev

tellStatus :: TaskWidget t m r e => R.Event t Status -> m ()
tellStatus ev = do
  task <- getTaskInfos ^. mapping #task
  tellTask $ flip (#status .~) task <$> ev

parentButton :: forall t m r e. TaskWidget t m r e => m ()
parentButton = do
  task <- getTaskInfos ^. mapping #task
  when (isn't (#partof % _Nothing) task) $ do
    event <- button "edit" (icon "" "layers_clear")
    tellTask $ (#partof .~ Nothing $ task) <$ event

deleteButton :: forall t m r e. TaskWidget t m r e => m ()
deleteButton = do
  task <- getTaskInfos ^. mapping #task
  deleteWidget $ task ^. #status
 where
  deleteWidget :: Status -> m ()
  deleteWidget (Status.Deleted time) = do
    event <- dateSelectionWidget "deleted" $ Just time
    tellStatus $ maybe Status.Pending Status.Deleted <$> event
  deleteWidget _ =
    button "edit" (icon "" "delete") >>= tellStatusByTime Status.Deleted

completedWidget :: forall t m r e. TaskWidget t m r e => m ()
completedWidget = do
  status <- getTaskInfos ^. mapping (#task % #status)
  whenJust (status ^? #_Completed) $ \time -> do
    event <- dateSelectionWidget "completed" $ Just time
    tellStatus $ maybe Status.Pending Status.Completed <$> event

statusWidget :: forall t m r e. TaskWidget t m r e => m ()
statusWidget = do
  status <- getTaskInfos <&> (\t -> (t ^. #status, t ^. #blocked))
  widget . widgetState $ status
 where
  widget :: (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status)) -> m ()
  widget (iconName, showClass, handlerMay) = do
    let (altIcon, handler) = case handlerMay of
          Nothing -> (D.blank, const D.blank)
          Just (altIconLabel, altClass, handlerPure) ->
            ( D.elClass "i" ("material-icons " <> altClass <> " showable") $
                D.text altIconLabel
            , tellStatusByTime handlerPure . D.domEvent D.Mouseup
            )
    (el, ()) <- D.elAttr' "div" ("class" =: "checkbox") $ do
      D.elClass
        "i"
        ( "material-icons " <> showClass
            <> if isJust handlerMay
              then " hideable"
              else ""
        )
        $ D.text iconName
      altIcon
    handler el
  widgetState ::
    (Status, Bool) ->
    (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status))
  widgetState = \case
    (Status.Pending, False) ->
      ("done", "hide", Just ("done", "grey", Status.Completed))
    (Status.Pending, True) ->
      ("block", "show", Just ("done", "grey", Status.Completed))
    (Status.Completed{}, _) ->
      ("done", "show", Just ("done", "hide", const Status.Pending))
    (Status.Deleted{}, _) ->
      ("delete", "show", Just ("done", "hide", const Status.Pending))
    (Status.Recurring{}, _) -> ("repeat", "show", Nothing)

collapseButton :: forall t m r e. TaskWidget t m r e => m ()
collapseButton = do
  taskInfos <- getTaskInfos
  hasChildren <-
    R.holdUniqDyn . fmap (any (has (#task % #status % #_Pending)))
      =<< lookupTasksM
        (taskInfos ^. #children)
  D.dyn_ $
    when <$> hasChildren ?? do
      open <- getIsExpanded $ taskInfos ^. #uuid
      let label = \case
            True -> "unfold_less"
            False -> "unfold_more"
      buttonEvent <-
        button "slimButton" $
          D.dyn_ (icon "collapse" . label <$> open)
      tellToggle $ taskInfos ^. #uuid <$ buttonEvent
