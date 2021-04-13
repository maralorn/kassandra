{-# LANGUAGE BlockArguments #-}

module Kassandra.AgendaWidget (agendaWidget) where

import qualified Data.Sequence as Seq
import Kassandra.BaseWidgets (button, icon)
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
import Kassandra.ListElementWidget (AdhocContext (..), definitionElementWidget, tellList)
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types (StandardWidget, Widget, getAppState)
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
            D.el "br" pass
            icon "" "room"
            D.text l
          whenJust comment \c -> do
            D.el "br" pass
            icon "" "comment"
            D.text c
          D.el "br" pass
          icon "" "schedule"
          printEventTime time
          D.el "br" pass
          icon "" "list"
          D.text calendarName
          D.el "br" pass
          calendarListWidget uid todoList

calendarListWidget :: StandardWidget t m r e => Text -> CalendarList -> m ()
calendarListWidget uid calendarList = do
  forM_
    (entries calendarList)
    ( (>> D.el "br" pass) . definitionElementWidget (AgendaEvent uid calendarList)
    )
  newTaskEvent <-
    createTextWidget
      (button "selector" $ D.text "New Task")
  tellList uid $ newTaskEvent <&> \content -> (#entries %~ (Seq.|> ListElement (AdHocTask content))) calendarList

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
