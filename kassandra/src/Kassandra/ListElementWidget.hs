module Kassandra.ListElementWidget (
  definitionElementWidget,
  AdhocContext (..),
  tellList,
) where

import qualified Data.Set as Set
import Kassandra.BaseWidgets (button)
import Kassandra.Calendar (CalendarList, completed)
import Kassandra.Config
import Kassandra.TaskWidget
import Kassandra.Types (AppStateChange, DataChange (SetEventList), StandardWidget)
import Kassandra.Util (tellSingleton)
import qualified Reflex.Dom as D

data AdhocContext = NoContext | AgendaEvent Text CalendarList | AgendaList Text (Set Text)

listElementWidget :: StandardWidget t m r e => AdhocContext -> ListItem -> m ()
listElementWidget context = \case
  TaskwarriorTask uuid -> uuidWidget taskTreeWidget (pure uuid)
  AdHocTask t -> adhocTaskWidget t context
  HabiticaTask _ -> error "HabiticaTasks are not yet supported"
  Mail _ -> error "Mails are not yet supported"

configListWidget :: StandardWidget t m r e => AdhocContext -> Text -> Maybe Natural -> m ()
configListWidget _context _ _ = error "ConfigLists are not implemented"

definitionElementWidget :: StandardWidget t m r e => AdhocContext -> DefinitionElement -> m ()
definitionElementWidget context = \case
  ConfigList name limit -> configListWidget context name limit
  ListElement el -> listElementWidget context el

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
