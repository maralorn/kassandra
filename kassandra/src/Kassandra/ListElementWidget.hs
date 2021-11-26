{-# LANGUAGE BlockArguments #-}

module Kassandra.ListElementWidget (
  definitionElementWidget,
  configListWidget,
  AdhocContext (..),
  tellList,
  queryWidget,
  selectWidget,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Kassandra.BaseWidgets (br, button, icon)
import Kassandra.Calendar (CalendarList, completed)
import Kassandra.Config (
  DefinitionElement (
    ChildrenList,
    ConfigList,
    DependenciesList,
    HabiticaList,
    ListElement,
    Mails,
    QueryList,
    TagList
  ),
  ListItem (AdHocTask, HabiticaTask, Mail, TaskwarriorTask),
  ListQuery,
  NamedListQuery (NamedListQuery),
 )
import Kassandra.DragAndDrop (tellSelected)
import Kassandra.ReflexUtil (smartSimpleList)
import Kassandra.Sorting (SortMode (SortModeTag), sortTasks)
import Kassandra.TaskWidget (
  taskList,
  taskTreeWidget,
  uuidWidget,
 )
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types (AppStateChange, DataChange (SetEventList), StandardWidget, TaskInfos, TaskState, getAppState, getSelectState, getTasks)
import Kassandra.Util (tellNewTask, tellSingleton)
import qualified Reflex as R
import qualified Reflex.Dom as D

data AdhocContext = NoContext | AgendaEvent Text CalendarList | AgendaList Text (Set Text)

selectWidget :: StandardWidget t m r e => DefinitionElement -> m ()
selectWidget definitionElement = do
  (dragEl, _) <- D.elClass' "span" "button" $ icon "" "filter_list"
  selectStateB <- toggleContainElement definitionElement <<$>> R.current <$> getSelectState
  tellSelected $ R.tag selectStateB (D.domEvent D.Click dragEl)
 where
  toggleContainElement :: DefinitionElement -> Seq DefinitionElement -> Seq DefinitionElement
  toggleContainElement entry selectedTasks =
    Seq.findIndexL (== entry) selectedTasks & maybe (selectedTasks |> entry) (`Seq.deleteAt` selectedTasks)

listElementWidget :: StandardWidget t m r e => AdhocContext -> ListItem -> m ()
listElementWidget context = \case
  TaskwarriorTask uuid -> uuidWidget taskTreeWidget (pure uuid)
  AdHocTask t -> adhocTaskWidget t context
  HabiticaTask _ -> error "HabiticaTasks are not yet supported"
  Mail _ -> error "Mails are not yet supported"

configListWidget :: forall t m r e. StandardWidget t m r e => AdhocContext -> Text -> Maybe Natural -> m ()
configListWidget context name limit = do
  D.text name >> br
  namedListQueries <- getAppState ^. mapping (#uiConfig % mapping #configuredLists)
  D.dyn_ $ namedListQueries <&> maybe (D.text [i|No list with name "#{name}" configured.|]) (queryWidget context) . getWidget
 where
  getWidget = Seq.lookup 0 . mapMaybe f
  f (NamedListQuery x query) | x == name = Just query
  f _ = Nothing

queryWidget :: StandardWidget t m r e => AdhocContext -> ListQuery -> m ()
queryWidget context els = smartSimpleList ((>> br) . definitionElementWidget context) (pure els)

tasksToShow :: Text -> TaskState -> Seq TaskInfos
tasksToShow tag = filter inList . fromList . HashMap.elems
 where
  inList :: TaskInfos -> Bool
  inList ((^. #task) -> task) = tag `Set.member` (task ^. #tags) && has (#status % #_Pending) task

definitionElementWidget :: StandardWidget t m r e => AdhocContext -> DefinitionElement -> m ()
definitionElementWidget context el = do
  selectWidget el
  el & \case
    ConfigList name limit -> configListWidget context name limit
    ListElement el' -> listElementWidget context el'
    QueryList query -> D.text "QueryLists not implemented"
    (TagList tag) ->
      do
        D.text tag
        tasks <- getTasks
        let showTasks = tasksToShow tag <$> tasks
        let sortMode = SortModeTag tag
        taskList
          (R.constant sortMode)
          (sortTasks sortMode <$> showTasks)
          (R.constDyn IsEmpty)
          taskTreeWidget
        tellNewTask . fmap (,#tags %~ Set.insert tag)
          =<< createTextWidget
            (button "selector" $ D.text "Add task to list")
    (ChildrenList uuid) -> D.text "ChildrenList not implemented"
    (DependenciesList uuid) -> D.text "DependenciesList not implemented"
    (HabiticaList list) -> D.text "HabiticaList not implemented"
    Mails -> D.text "Mails not implemented"

adhocTaskWidget :: StandardWidget t m r e => Text -> AdhocContext -> m ()
adhocTaskWidget description = \case
  AgendaEvent uid calendarList -> do
    changeDoneStatus <- checkBox (completed calendarList)
    D.text [i| #{description}|]
    tellList uid $
      changeDoneStatus <&> \case
        True -> (#completed %~ Set.insert description) calendarList
        False -> (#completed %~ Set.delete description) calendarList
  AgendaList _ completed -> do
    checkBox completed >> text
  NoContext ->
    checkBox mempty >> text
 where
  checkBox completed = if description `Set.member` completed then (False <$) <$> button "" (D.text "[x]") else (True <$) <$> button "" (D.text "[ ]")
  text = D.text [i| #{description}|]

tellList :: StandardWidget t m r e => Text -> D.Event t CalendarList -> m ()
tellList uid listEvent = tellSingleton $ (_Typed @AppStateChange % _Typed @DataChange #) . SetEventList uid <$> listEvent
