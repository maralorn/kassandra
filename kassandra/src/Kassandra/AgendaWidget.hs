{-# LANGUAGE BlockArguments #-}

module Kassandra.AgendaWidget (agendaWidget) where

import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import Kassandra.BaseWidgets (br, button, icon)
import Kassandra.Calendar (
  CalendarEvent (
    CalendarEvent,
    calendarName,
    comment,
    description,
    location,
    time,
    todoList,
    uid
  ),
  CalendarList (entries),
  EventTime (AllDayEvent, SimpleEvent),
  switchToCurrentZone,
 )
import Kassandra.Config (DefinitionElement (..), ListItem (..))
import Kassandra.DragAndDrop (insertArea)
import Kassandra.ListElementWidget (AdhocContext (..), definitionElementWidget, tellList)
import Kassandra.ReflexUtil (listWithGaps)
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types (StandardWidget, Widget, getAppState)
import qualified Reflex as R
import qualified Reflex.Dom as D

agendaWidget :: StandardWidget t m r e => m ()
agendaWidget = do
  appState <- getAppState
  let calendarEvents = appState ^. #calendarEvents
  void $
    D.dyn_ $
      calendarEvents <&> mapM_ \CalendarEvent{description, time, calendarName, location, comment, uid, todoList} ->
        D.divClass "event" $ do
          icon "" "event"
          D.text description
          whenJust location \l -> do
            br
            icon "" "room"
            D.text l
          whenJust comment \c -> do
            br
            icon "" "comment"
            D.text c
          br
          icon "" "schedule"
          printEventTime time
          br
          icon "" "list"
          D.text calendarName
          br
          calendarListWidget uid todoList

calendarListWidget :: StandardWidget t m r e => Text -> CalendarList -> m ()
calendarListWidget uid calendarList = do
  listWithGaps widget gapWidget (pure (entries calendarList))
  newTaskEvent <- createTextWidget (button "selector" $ D.text "New Task")
  tellList uid $ newTaskEvent <&> \content -> (#entries %~ (Seq.|> ListElement (AdHocTask content))) calendarList
 where
  widget definitionElement = D.divClass "definitionElement" do
    D.divClass "definitionUI" $ do
      delete <- button "" (D.text "x")
      tellList uid $ delete $> (#entries %~ Seq.filter (definitionElement /=)) calendarList
    D.divClass "element" $ definitionElementWidget (AgendaEvent uid calendarList) definitionElement
  gapWidget around = do
    evs <- insertArea (pure mempty) $ icon "dropHere above" "forward"
    tellList uid $ R.attachWith (flip insertedCalendarList) (R.current around) evs
  insertedCalendarList toInsert = \case
    (Nothing, Nothing) -> updateOnList (const (toSeq toInsert))
    (Just _, Nothing) -> updateOnList (<> toSeq toInsert)
    (Nothing, Just _) -> updateOnList (toSeq toInsert <>)
    (Just _, Just after) -> updateOnList ((\(a, b) -> a <> toSeq toInsert <> b) . Seq.breakl (== after))
   where
    updateOnList upd = (#entries %~ upd . Seq.filter (isNothing . flip NESeq.elemIndexL toInsert)) calendarList

printEventTime :: Widget t m => EventTime -> m ()
printEventTime (SimpleEvent start end) = do
  showstart <- switchToCurrentZone (start ^. #time)
  showend <- switchToCurrentZone (end ^. #time)
  let printFullTime = toText . formatTime defaultTimeLocale "%a %Y-%m-%d %H:%M"
  let printEndTime = toText . formatTime defaultTimeLocale "%H:%M"
  let zone = start ^. #zone
  D.text (printFullTime showstart)
  icon "" "arrow_right_alt"
  D.text (printEndTime showend)
  D.text [i|(Defined in #{zone})|]
printEventTime (AllDayEvent startDay endDay) = do
  let printFullTime = toText . formatTime defaultTimeLocale "%a %Y-%m-%d"
  D.text (printFullTime startDay)
  when (startDay /= endDay) do
    icon "" "arrow_right_alt"
    D.text (printFullTime endDay)
printEventTime _ = pass
