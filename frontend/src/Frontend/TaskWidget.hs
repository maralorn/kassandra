{-# LANGUAGE BlockArguments, TypeApplications, TupleSections, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, TemplateHaskell, FlexibleInstances, FunctionalDependencies, MultiWayIf, OverloadedLabels  #-}
module Frontend.TaskWidget
  ( taskWidget
  , taskList
  )
where

import qualified Data.Text                     as Text
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.UDA                ( UDA )
import           Frontend.Util                  ( lookupCurrentDyn
                                                , tellSingleton
                                                , lookupCurrent
                                                , lookupTasksM
                                                , tellTask
                                                )
import           Frontend.Types                 ( getTime
                                                , DragState(NoDrag, DraggedTask)
                                                , AppChange(DragChange)
                                                , DataChange
                                                  ( ToggleEvent
                                                  , CreateTask
                                                  )
                                                , StandardWidget
                                                , TaskInfos
                                                , AppState
                                                , getAppState
                                                , al
                                                , fl
                                                )
import           Frontend.TextEditWidget        ( lineWidget
                                                , createTextWidget
                                                )
import           Frontend.BaseWidgets           ( button
                                                , icon
                                                )
import           Frontend.Sorting               ( sortTasks
                                                , SortMode(SortModePartof)
                                                , SortPosition(SortPosition)
                                                )
import           Frontend.DragAndDrop           ( taskDropArea
                                                , childDropArea
                                                )
import           Frontend.TimeWidgets           ( dateSelectionWidget )

--type ExpandedTasks = HashSet UUID
type TaskWidget t m r = (StandardWidget t m r, HasType (TaskInfos) r)
type TaskMonad m r = (MonadReader r m, HasType (TaskInfos) r)

instance LabelOptic "taskInfos" A_Lens (a, TaskInfos) (a, TaskInfos) TaskInfos TaskInfos where
  labelOptic = _2

getTaskInfos :: TaskMonad m r => m (TaskInfos)
getTaskInfos = ask ^. al typed

getChildren :: TaskWidget t m r => m (R.Dynamic t [TaskInfos])
getChildren = getTaskInfos ^. al #children >>= lookupCurrent

taskWidget
  :: forall t m r . (StandardWidget t m r) => R.Dynamic t TaskInfos -> m ()
taskWidget taskInfos' = D.divClass "task" $ do
  taskInfosD <- R.holdUniqDyn taskInfos'
  appState'  <- getAppState
  D.dyn_ $ taskInfosD <&> \taskInfos ->
    runReaderT widgets (appState', taskInfos)
  childrenWidget taskInfosD
 where
  widgets :: ReaderT (AppState t, TaskInfos) m ()
  widgets = do
    D.divClass "uppertask" $ do
      D.divClass "statusWrapper" statusWidget
      D.divClass "righttask" $ do
        collapseButton
        dropChildWidget
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

pathWidget :: (TaskWidget t m r) => m ()
pathWidget = do
  parents <- getTaskInfos ^. al #parents >>= lookupTasksM ^. fl #description
  D.dyn_ $ flip whenNotNull showPath <$> parents
 where
  showPath :: TaskWidget t m r => NonEmpty Text -> m ()
  showPath parents = D.elClass "span" "parentPath" $ do
    D.el "br" $ pass
    makePath parents

makePath :: (TaskWidget t m r) => NonEmpty Text -> m ()
makePath =
  D.elClass "span" "path" . D.text . Text.intercalate " ≫ " . reverse . toList

br :: D.DomBuilder t m => m ()
br = D.el "br" pass

dependenciesWidget :: (TaskWidget t m r) => m ()
dependenciesWidget = do
  taskInfos  <- getTaskInfos
  revDepends <- lookupTasksM $ taskInfos ^. #revDepends
  depends    <- lookupTasksM $ taskInfos ^. #depends
  D.dyn_ $ whenNotNull <$> depends <*> pure
    (\ds -> do
      br
      D.text "dependencies: "
      forM_ ds $ \d -> do
        makeOwnPath d
        deleteEvent <- button "edit" $ icon "" "delete"
        tellTask $ (removeDep (taskInfos ^. #task) d) <$ deleteEvent
        br
    )
  D.dyn_ $ whenNotNull <$> revDepends <*> pure
    (\rds -> do
      br
      D.text "reverse dependencies: "
      forM_ rds $ \rd -> do
        makeOwnPath rd
        deleteEvent <- button "edit" $ icon "" "delete"
        tellTask $ (removeDep (rd ^. #task) taskInfos) <$ deleteEvent
        br
    )
 where
  removeDep
    :: ( Is k1 A_Getter
       , Is k2 A_Setter
       , LabelOptic "depends" k2 s1 t [UUID] [UUID]
       , LabelOptic "uuid" k1 s2 s2 UUID UUID
       )
    => s1
    -> s2
    -> t
  removeDep task dependency =
    #depends %~ filter (dependency ^. #uuid /=) $ task

makeOwnPath :: (TaskWidget t m r) => TaskInfos -> m ()
makeOwnPath task =
  D.dyn_
    .   fmap (makePath . (\ps -> task ^. #description :| ps))
    =<< lookupTasksM (task ^. #parents)
    ^.  #description

dropChildWidget :: (TaskWidget t m r) => m ()
dropChildWidget = do
  taskInfos <- getTaskInfos
  childrenD <- getChildren
  when (not $ taskInfos ^. #showChildren)
    $ childDropArea
        (SortPosition (SortModePartof <$> taskInfos ^. #uuid % to R.constant)
                      (childrenD ^. al (al #task) % #current)
                      (R.constant Nothing)
        )
        (R.constDyn
          (taskInfos ^. #uuid : taskInfos ^. #parents <> taskInfos ^. #children)
        )
    $ icon "dropHere" "move_to_inbox"
  taskDropArea (taskInfos ^. #uuid % to (R.constDyn . one))
               (icon "dropHere plusOne" "block")
    $ fmap
        (\dependency ->
          one $ #depends %~ (dependency ^. #uuid :) $ taskInfos ^. #task
        )
  taskDropArea (taskInfos ^. #uuid % to (R.constDyn . one))
               (icon "dropHere plusTwo" "schedule")
    $ fmap (\task -> one $ #depends %~ (taskInfos ^. #uuid :) $ task ^. #task)


tagsWidget :: forall t m r . TaskWidget t m r => m ()
tagsWidget = do
  task <- getTaskInfos ^. al #task
  forM_ (task ^. #tags) $ \tag -> D.elClass "span" "tag" $ do
    D.text tag
    deleteEvent <- button "edit" $ icon "" "delete"
    tellTask $ (#tags %~ filter (tag /=) $ task) <$ deleteEvent
  tagEvent <- createTextWidget . button "edit" $ icon "" "add_circle"
  tellTask $ (\tag -> #tags %~ (tag :) $ task) <$> tagEvent

getNewUDA :: forall t m r . TaskWidget t m r => m (UDA)
getNewUDA = one . ("partof", ) . toJSON <$> getTaskInfos ^. al #uuid

addChildWidget :: TaskWidget t m r => m ()
addChildWidget = do
  descriptionEvent <- createTextWidget . button "edit" $ icon "" "add_circle"
  newUDA           <- getNewUDA
  tellSingleton
    $   (\description -> Right $ CreateTask description (#uda .~ newUDA))
    <$> descriptionEvent

childrenWidget
  :: forall t m r . StandardWidget t m r => R.Dynamic t (TaskInfos) -> m ()
childrenWidget taskInfosD = do
  showChildren <- R.holdUniqDyn $ taskInfosD ^. #showChildren
  D.dyn_ $ showOptional <$> showChildren
 where
  showOptional :: Bool -> m ()
  showOptional x = when x $ do
    children <- R.holdUniqDyn =<< lookupCurrentDyn =<< R.holdUniqDyn
      (taskInfosD ^. al #children)
    let sortModeD = SortModePartof <$> taskInfosD ^. #uuid
    blacklist <- R.holdUniqDyn
      $ liftA2 (:) (taskInfosD ^. #uuid) (taskInfosD ^. al #parents)
    sortedList <- R.holdUniqDyn $ (sortTasks <$> sortModeD <*> children)
    D.divClass "children"
      $ taskList (sortModeD ^. #current) sortedList blacklist

taskList
  :: StandardWidget t m r
  => R.Behavior t SortMode
  -> R.Dynamic t [TaskInfos]
  -> R.Dynamic t [UUID]
  -> m ()
taskList mode childrenD blacklistD = do
  let partialSortPosition =
        SortPosition mode (childrenD ^. al (al #task) % #current)
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
        childDropArea
            (partialSortPosition (Just <$> currentUuidD ^. #current))
            (ignoreD <> blacklistD)
          $ icon "dropHere above" "forward"
        taskWidget $ childD ^. fl _1
  let ignoreD = (^.. folded) . lastOf folded <$> childrenD ^. #uuid
  childDropArea (partialSortPosition (R.constant Nothing))
                (ignoreD <> blacklistD)
    $ icon "dropHere above" "forward"


waitWidget :: forall t m r . TaskWidget t m r => m ()
waitWidget = do
  event <-
    getTaskInfos >>= ((^? #status % #_Waiting) >>> dateSelectionWidget "wait")
  tellStatus $ maybe Status.Pending Status.Waiting <$> event

dueWidget :: TaskWidget t m r => m ()
dueWidget = do
  task  <- getTaskInfos ^. al #task
  event <- dateSelectionWidget "due" $ task ^. #due
  tellTask $ flip (#due .~) task <$> event

descriptionWidget :: TaskWidget t m r => m ()
descriptionWidget = do
  task        <- getTaskInfos ^. al #task
  (dragEl, _) <- D.elAttr' "span" ("draggable" =: "true") $ do
    icon "edit slimButton" "open_with"
  event <- lineWidget $ task ^. #description
  tellSingleton
    $  (Left . DragChange . DraggedTask $ task ^. #uuid)
    <$ D.domEvent D.Dragstart dragEl
  tellSingleton $ (Left . DragChange $ NoDrag) <$ D.domEvent D.Dragend dragEl
  tellTask $ (flip (#description .~) task) <$> event

tellStatusByTime
  :: TaskWidget t m r => ((UTCTime -> Status) -> R.Event t a -> m ())
tellStatusByTime handler ev = do
  time <- getTime
  tellStatus $ handler . zonedTimeToUTC <$> R.tag (R.current time) ev

tellStatus :: TaskWidget t m r => R.Event t Status -> m ()
tellStatus ev = do
  task <- getTaskInfos ^. al #task
  tellTask $ (flip (#status .~) task) <$> ev

parentButton :: forall t m r . TaskWidget t m r => m ()
parentButton = do
  task <- getTaskInfos ^. al #task
  when (isn't (#partof % _Nothing) task) $ do
    event <- button "edit" (icon "" "layers_clear")
    tellTask $ (#partof .~ Nothing $ task) <$ event

deleteButton :: forall t m r . TaskWidget t m r => m ()
deleteButton = do
  task <- getTaskInfos ^. al #task
  deleteWidget $ task ^. #status
 where
  deleteWidget :: Status -> m ()
  deleteWidget (Status.Deleted time) = do
    event <- dateSelectionWidget "deleted" $ Just time
    tellStatus $ maybe Status.Pending Status.Deleted <$> event
  deleteWidget _ =
    button "edit" (icon "" "delete") >>= tellStatusByTime Status.Deleted

completedWidget :: forall t m r . TaskWidget t m r => m ()
completedWidget = do
  status <- getTaskInfos ^. al #status
  whenJust (status ^? #_Completed) $ \time -> do
    event <- dateSelectionWidget "completed" $ Just time
    tellStatus $ maybe Status.Pending Status.Completed <$> event

statusWidget :: forall t m r . TaskWidget t m r => m ()
statusWidget = do
  status <- getTaskInfos <&> (\t -> (t ^. #status, t ^. #blocked))
  widget . widgetState $ status
 where
  widget :: (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status)) -> m ()
  widget (iconName, showClass, handlerMay) = do
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
    :: (Status, Bool)
    -> (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status))
  widgetState = \case
    (Status.Pending, False) ->
      ("done", "hide", Just ("done", "grey", Status.Completed))
    (Status.Pending, True) ->
      ("block", "show", Just ("done", "grey", Status.Completed))
    (Status.Completed{}, _) ->
      ("done", "show", Just ("done", "hide", const Status.Pending))
    (Status.Deleted{}, _) ->
      ("delete", "show", Just ("done", "hide", const Status.Pending))
    (Status.Waiting{}, _) ->
      ("schedule", "show", Just ("done", "grey", Status.Completed))
    (Status.RecurringParent{}, _) -> ("repeat", "show", Nothing)
    (Status.RecurringChild{} , _) -> ("repeat", "show", Nothing)


collapseButton :: forall t m r . TaskWidget t m r => m ()
collapseButton = do
  taskInfos <- getTaskInfos
  when (not . null $ taskInfos ^. #children) $ do
    let open  = taskInfos ^. #showChildren
        label = if open then "unfold_less" else "unfold_more"
    buttonEvent <- button "slimButton" $ icon "collapse" label
    tellSingleton
      $  (Right $ ToggleEvent (taskInfos ^. #uuid) (not open))
      <$ buttonEvent
