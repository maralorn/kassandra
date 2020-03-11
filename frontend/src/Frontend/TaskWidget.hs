{-# LANGUAGE BlockArguments, TypeApplications, TupleSections, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, TemplateHaskell, FlexibleInstances, FunctionalDependencies, MultiWayIf, OverloadedLabels  #-}
module Frontend.TaskWidget
  ( taskWidget
  , taskList
  )
where
import qualified Data.HashMap.Strict           as HashMap
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Taskwarrior.Task              as Task
import           Taskwarrior.Task               ( Task )
import           Taskwarrior.UDA                ( UDA )
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.Status             ( Status )
import           Data.Time                      ( UTCTime )
import           Data.Time.LocalTime            ( zonedTimeToUTC )
import           Data.UUID                      ( UUID )
import qualified Data.Aeson                    as Aeson
import           Frontend.Util
import           Frontend.Types
import           Frontend.TextEditWidget


newtype DynTask t = TaskDyn { _toDynTask :: R.Dynamic t TaskInfos }

makeClassy ''DynTask
type TaskWidget t m r = (StandardWidget t m r, HasDynTask r t)
type TaskMonad t m r = (R.Reflex t, MonadReader r m, HasDynTask r t)

instance HasDynTask (AppState t, DynTask t) t where
  dynTask = _2
instance HasAppState (AppState t, DynTask t) t where
  appState = _1

getTaskInfos :: TaskMonad t m r => m (R.Dynamic t TaskInfos)
getTaskInfos = (^. toDynTask) <$> ask

getChildren :: (TaskWidget t m r) => m (R.Dynamic t [TaskInfos])
getChildren = do
  tasks              <- getTasks
  taskInfos          <- (^. toDynTask) <$> ask
  unfilteredChildren <- R.holdUniqDyn $ R.zipDynWith
    (mapMaybe <$> flip HashMap.lookup)
    tasks
    (taskInfos ^. #children)
  filterCurrent unfilteredChildren

taskWidget
  :: forall t m r . (StandardWidget t m r) => R.Dynamic t TaskInfos -> m ()
taskWidget taskInfos' = D.divClass "task" $ do
  taskInfos <- R.holdUniqDyn taskInfos'
  appState' <- getAppState
  runReaderT widgets (appState', TaskDyn taskInfos)
 where
  widgets :: ReaderT (AppState t, DynTask t) m ()
  widgets = do
    statusWidget
    collapseButton
    dropChildWidget
    descriptionWidget
    tagsWidget
    waitWidget
    dueWidget
    deleteButton
    parentButton
    childrenWidget
    completedWidget

dropChildWidget :: (TaskWidget t m r) => m ()
dropChildWidget = do
  taskInfosD <- getTaskInfos
  childrenD  <- getChildren
  let blacklistD =
        (:)
          <$> (taskInfosD ^. #uuid)
          <*> (taskInfosD ^. #parents)
          <>  (taskInfosD ^. #children)
      dropArea =
        taskDropArea
            (SortPosition (SortModePartof <$> taskInfosD ^. #uuid % #current)
                          (childrenD ^. #task % #current)
                          (R.constant Nothing)
            )
            blacklistD
          $ icon "dropHere" "move_to_inbox"
      showDropArea = \case
        True  -> dropArea
        False -> D.blank
  D.dyn_ $ taskInfosD ^. (#showChildren % to (showDropArea . not <$>))

tagsWidget :: forall t m r . TaskWidget t m r => m ()
tagsWidget = do
  task <- getTaskInfos ^. #task
  D.dyn_ $ tagList <$> task
  tagEvent <- createTextWidget . button "edit" $ icon "" "add_circle"
  tellTask (\task' tag -> task' { Task.tags = tag : Task.tags task' }) tagEvent
 where
  tagList :: Task -> m ()
  tagList Task.Task { Task.tags } = forM_ tags $ \tag ->
    D.elClass "span" "tag" $ do
      D.text tag
      deleteEvent <- button "" $ icon "" "delete"
      tellTask
        (\task () -> task { Task.tags = filter (tag /=) $ Task.tags task })
        deleteEvent

getNewUDA :: forall t m r . TaskWidget t m r => m (R.Behavior t UDA)
getNewUDA =
  (one . ("partof", ) . Aeson.toJSON . Task.uuid <$>)
    .   R.current
    <$> getTaskInfos
    ^.  #task

childrenWidget :: forall t m r . TaskWidget t m r => m ()
childrenWidget = do
  showChilds <- getTaskInfos ^. #showChildren
  D.dyn_ $ showOptional <$> showChilds
  descriptionEvent <- createTextWidget . button "edit" $ icon "" "add_circle"
  newUDA           <- getNewUDA
  tellSingleton
    . R.attachWith
        (\uda description ->
          Right $ CreateTask description (\t -> t { Task.uda })
        )
        newUDA
    $ descriptionEvent
 where
  showOptional :: Bool -> m ()
  showOptional x = when x $ do
    taskInfosD <- getTaskInfos
    childrenD  <- getChildren
    let sortModeD  = SortModePartof <$> taskInfosD ^. #uuid
        blacklistD = (:) <$> (taskInfosD ^. #uuid) <*> (taskInfosD ^. #parents)
    D.divClass "children" $ taskList (sortModeD ^. #current)
                                     (sortTasks <$> sortModeD <*> childrenD)
                                     blacklistD

taskList
  :: StandardWidget t m r
  => R.Behavior t SortMode
  -> R.Dynamic t [TaskInfos]
  -> R.Dynamic t [UUID]
  -> m ()
taskList mode childrenD blacklistD = do
  let partialSortPosition = SortPosition mode (childrenD ^. #task % #current)
  void
    $ R.simpleList ((\xs -> zip xs (Nothing : fmap Just xs)) <$> childrenD)
    $ \childD -> do
        let
          currentUuidD = childD ^. fl _1 % #uuid
          ignoreD =
            ((:) <$> currentUuidD <*>)
              $   (^.. folded)
              <$> childD
              ^.  fl _2
              %   #uuid
        taskDropArea (partialSortPosition (Just <$> currentUuidD ^. #current))
                     (ignoreD <> blacklistD)
          $ icon "dropHere above" "forward"
        taskWidget $ childD ^. fl _1
  let ignoreD = (^.. folded) . lastOf folded <$> childrenD ^. #uuid
  taskDropArea (partialSortPosition (R.constant Nothing))
               (ignoreD <> blacklistD)
    $ icon "dropHere above" "forward"

tellTask :: TaskWidget t m r => (Task -> a -> Task) -> R.Event t a -> m ()
tellTask handler ev = do
  task <- getTaskInfos ^. #task % fl #current
  tellSingleton . R.attachWith (((Right . ChangeTask) .) . handler) task $ ev

waitWidget :: forall t m r . TaskWidget t m r => m ()
waitWidget = do
  status <- getTaskInfos ^. #status
  event  <- dateSelectionWidget "wait" $ (^? #_Waiting) <$> status
  tellTask (flip (#status .~)) $ maybe Status.Pending Status.Waiting <$> event

dueWidget :: TaskWidget t m r => m ()
dueWidget = do
  task  <- getTaskInfos ^. #task
  event <- dateSelectionWidget "due" $ Task.due <$> task
  tellTask (flip (#due .~)) event

descriptionWidget :: TaskWidget t m r => m ()
descriptionWidget = do
  task            <- getTaskInfos ^. #task
  (dragEl, event) <- D.elAttr' "span" ("draggable" =: "true") $ do
    icon "" "open_with"
    lineWidget $ task ^. #description
  tellSingleton
    $ R.tag (Left . DragChange . DraggedTask . Task.uuid <$> R.current task)
    $ D.domEvent D.Dragstart dragEl
  tellSingleton $ (Left . DragChange $ NoDrag) <$ D.domEvent D.Dragend dragEl
  tellTask (flip (#description .~)) event

tellStatusByTime
  :: TaskWidget t m r => ((UTCTime -> Status) -> R.Event t a -> m ())
tellStatusByTime handler ev = do
  time <- getTime
  tellStatus $ handler . zonedTimeToUTC <$> R.tag (R.current time) ev

tellStatus :: TaskWidget t m r => R.Event t Status -> m ()
tellStatus = tellTask (\task status -> task { Task.status })

parentButton :: forall t m r . TaskWidget t m r => m ()
parentButton = do
  task <- getTaskInfos ^. #task
  D.dyn_ $ widget . isn't (#partof % _Nothing) <$> task
 where
  widget :: Bool -> m ()
  widget x = when x $ do
    event <- button "edit" (icon "" "layers_clear")
    tellTask (\task () -> (#partof .~ Nothing) task) event

deleteButton :: forall t m r . TaskWidget t m r => m ()
deleteButton = do
  task <- getTaskInfos ^. #task
  D.dyn_ $ deleteWidget <$> task ^. #status
 where
  deleteWidget :: Status -> m ()
  deleteWidget (Status.Deleted time) = do
    event <- dateSelectionWidget "deleted" $ R.constDyn (Just time)
    tellStatus $ maybe Status.Pending Status.Deleted <$> event
  deleteWidget _ =
    button "edit" (icon "" "delete") >>= tellStatusByTime Status.Deleted

completedWidget :: forall t m r . TaskWidget t m r => m ()
completedWidget = do
  statusD <- getTaskInfos ^. #status
  D.dyn_ $ completedTime <$> statusD
 where
  completedTime :: Status -> m ()
  completedTime (Status.Completed time) = do
    event <- dateSelectionWidget "completed" $ R.constDyn (Just time)
    tellStatus $ maybe Status.Pending Status.Completed <$> event
  completedTime _ = D.blank

statusWidget :: forall t m r . TaskWidget t m r => m ()
statusWidget = do
  statusD <- getTaskInfos ^. #status
  D.dyn_ $ dynWidget . widgetState <$> statusD
 where
  dynWidget
    :: (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status)) -> m ()
  dynWidget (iconName, showClass, handlerMay) = do
    let (altIcon, handler) = case handlerMay of
          Nothing -> (D.blank, const D.blank)
          Just (altIconLabel, altClass, handlerPure) ->
            ( D.elClass "i" ("material-icons " <> altClass <> " showable")
              $ D.text altIconLabel
            , tellStatusByTime handlerPure . D.domEvent D.Mouseup
            )
    (el, ()) <- D.elAttr' "div" ("class" =: "checkbox") $ do
      D.elClass
          "i"
          ("material-icons " <> showClass <> if isJust handlerMay
            then " hideable"
            else ""
          )
        $ D.text iconName
      altIcon
    handler el
  widgetState
    :: Status -> (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status))
  widgetState = \case
    Status.Pending ->
      ("done", "hide", Just ("done", "grey", Status.Completed))
    Status.Completed{} ->
      ("done", "show", Just ("done", "hide", const Status.Pending))
    Status.Deleted{} ->
      ("delete", "show", Just ("done", "hide", const Status.Pending))
    Status.Waiting{} ->
      ("schedule", "show", Just ("done", "grey", Status.Completed))
    Status.RecurringParent{} -> ("repeat", "show", Nothing)
    Status.RecurringChild{}  -> ("repeat", "show", Nothing)


collapseButton :: forall t m r . TaskWidget t m r => m ()
collapseButton = do
  children <- getChildren
  D.dyn_ $ displayWidget . not . null <$> children where
  displayWidget True = do
    open <- getTaskInfos ^. #showChildren
    D.dyn_ $ buttonWidget <$> open
  displayWidget False = D.blank
  buttonWidget :: (Bool -> m ())
  buttonWidget open = do
    let label = if open then "unfold_less" else "unfold_more"
    buttonEvent <- button "" $ icon "collapse" label
    task        <- R.current <$> getTaskInfos ^. #task
    tellSingleton
      $   (\task' -> Right $ ToggleEvent (Task.uuid task') (not open))
      <$> R.tag task buttonEvent
