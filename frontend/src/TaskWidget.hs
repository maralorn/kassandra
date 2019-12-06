{-# LANGUAGE BlockArguments, TypeApplications, TupleSections, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, TemplateHaskell, FlexibleInstances, FunctionalDependencies  #-}
module TaskWidget
  ( taskWidget
  )
where
import           ClassyPrelude
import qualified Data.HashMap.Strict           as HashMap
import           Data.List.NonEmpty             ( NonEmpty )
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as D
import qualified Data.Maybe                    as Maybe
import qualified Reflex                        as R
import qualified Taskwarrior.Task              as Task
import           Taskwarrior.Task               ( Task )
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.Status             ( Status )
import           Types
import           TextEditWidget
import           Data.Time.LocalTime            ( zonedTimeToUTC )
import           Control.Lens.TH                ( makeClassy )
import           Control.Lens.Getter            ( (^.) )
import           Control.Lens.Combinators       ( _1
                                                , _2
                                                )
import           Data.Aeson                    as Aeson


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

getTask :: TaskMonad t m r => m (R.Dynamic t Task)
getTask = fmap task <$> getTaskInfos

getChildren :: (TaskWidget t m r) => m (R.Dynamic t [TaskInfos])
getChildren = do
  tasks     <- getTasks
  taskInfos <- (^. toDynTask) <$> ask
  R.holdUniqDyn
    $   (Maybe.mapMaybe <$> flip HashMap.lookup)
    <$> tasks
    <*> (children <$> taskInfos)

getShowChildren :: (TaskMonad t m r) => m (R.Dynamic t Bool)
getShowChildren = fmap showChildren <$> getTaskInfos

tellSingleton
  :: (R.Reflex t, R.EventWriter t (NonEmpty event) m) => R.Event t event -> m ()
tellSingleton = R.tellEvent . fmap singleton

taskWidget
  :: forall t m r . (StandardWidget t m r) => R.Dynamic t TaskInfos -> m ()
taskWidget taskInfos' = D.divClass "task" $ do
  taskInfos <- R.holdUniqDyn taskInfos'
  state     <- getAppState
  runReaderT widgets (state, TaskDyn taskInfos)
 where
  widgets :: ReaderT (AppState t, DynTask t) m ()
  widgets = do
    statusWidget
    collapseButton
    descriptionWidget
    waitWidget
    dueWidget
    deleteButton
    childrenWidget
    completedWidget

childrenWidget :: forall t m r . TaskWidget t m r => m ()
childrenWidget = do
  showChilds <- getShowChildren
  D.dyn_ $ showOptional <$> showChilds
  descriptionEvent <- createTextWidget . button "edit" $ icon "" "add"
  taskDyn          <- getTask
  tellSingleton $ R.attachPromptlyDynWith
    (\task description -> CreateTask
      description
      (\t -> t
        { Task.uda = HashMap.singleton "partof"
                                       (Aeson.toJSON . Task.uuid $ task)
        }
      )
    )
    taskDyn
    descriptionEvent
 where
  showOptional :: Bool -> m ()
  showOptional True = do
    children <- getChildren
    void $ D.divClass "children" $ R.simpleList children taskWidget
  showOptional False = D.blank

tellTask :: TaskWidget t m r => (Task -> a -> Task) -> R.Event t a -> m ()
tellTask handler ev = do
  task <- getTask
  tellSingleton . R.attachPromptlyDynWith ((ChangeTask .) . handler) task $ ev

waitWidget :: forall t m r . TaskWidget t m r => m ()
waitWidget = do
  taskDyn <- getTask
  let maybeWait =
        (\case
            Status.Waiting a -> Just a
            _                -> Nothing
          )
          .   Task.status
          <$> taskDyn
  event <- dateSelectionWidget "wait" maybeWait
  tellTask
    (\task time ->
      task { Task.status = maybe Status.Pending Status.Waiting time }
    )
    event

dueWidget :: TaskWidget t m r => m ()
dueWidget = do
  task  <- getTask
  event <- dateSelectionWidget "due" $ Task.due <$> task
  tellTask (\task' due -> task' { Task.due }) event

descriptionWidget :: TaskWidget t m r => m ()
descriptionWidget = do
  task  <- getTask
  event <- lineWidget $ Task.description <$> task
  tellTask (\task' description -> task' { Task.description }) event

tellStatusByTime
  :: TaskWidget t m r => ((UTCTime -> Status) -> R.Event t a -> m ())
tellStatusByTime handler ev = do
  time <- getTime
  tellStatus $ R.attachPromptlyDynWith
    (\time' _ -> handler $ zonedTimeToUTC time')
    time
    ev

tellStatus :: TaskWidget t m r => R.Event t Status -> m ()
tellStatus = tellTask (\task status -> task { Task.status })

deleteButton :: forall t m r . TaskWidget t m r => m ()
deleteButton = do
  task <- getTask
  D.dyn_ $ deleteWidget . Task.status <$> task
 where
  deleteWidget :: Status -> m ()
  deleteWidget (Status.Deleted time) = do
    event <- dateSelectionWidget "deleted" $ R.constDyn (Just time)
    tellStatus $ maybe Status.Pending Status.Deleted <$> event
  deleteWidget _ =
    button "edit" (icon "" "delete") >>= tellStatusByTime Status.Deleted

completedWidget :: forall t m r . TaskWidget t m r => m ()
completedWidget = do
  task <- getTask
  D.dyn_ $ completedTime . Task.status <$> task
 where
  completedTime :: Status -> m ()
  completedTime (Status.Completed time) = do
    event <- dateSelectionWidget "completed" $ R.constDyn (Just time)
    tellStatus $ maybe Status.Pending Status.Completed <$> event
  completedTime _ = D.blank

statusWidget :: forall t m r . TaskWidget t m r => m ()
statusWidget = do
  task <- getTask
  D.dyn_ $ dynWidget . widgetState . Task.status <$> task
 where
  dynWidget
    :: (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status)) -> m ()
  dynWidget (iconName, showClass, handlerMay) = do
    let (altIcon, handler) = case handlerMay of
          Nothing -> (D.blank, const D.blank)
          Just (altIconLabel, altClass, handlerPure) ->
            ( D.elClass "i" ("material-icons " ++ altClass ++ " showable")
              $ D.text altIconLabel
            , tellStatusByTime handlerPure . D.domEvent D.Mouseup
            )
    (el, ()) <- D.elAttr' "div" ("class" =: "checkbox") $ do
      D.elClass
          "i"
          ("material-icons " ++ showClass ++ if isJust handlerMay
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
    open <- getShowChildren
    D.dyn_ $ buttonWidget <$> open
  displayWidget False = D.blank
  buttonWidget :: (Bool -> m ())
  buttonWidget open = do
    let label = if open then "unfold_less" else "unfold_more"
    buttonEvent <- button "" $ icon "collapse" label
    task        <- getTask
    tellSingleton $ R.attachPromptlyDynWith
      (\task' _ -> ToggleEvent (Task.uuid task') (not open))
      task
      buttonEvent
