{-# LANGUAGE BlockArguments #-}

module Kassandra.AgendaWidget (agendaWidget) where

import Kassandra.Calendar
import Kassandra.Types
import qualified Reflex.Dom as D
import Kassandra.BaseWidgets

agendaWidget :: StandardWidget t m r e => m ()
agendaWidget = do
  appState <- getAppState
  let calendarEvents = appState ^. #calendarEvents
  void $
    D.dyn_ $
      calendarEvents <&> mapM_ \CalendarEvent{description, time, calendarName, location, comment} -> do
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

printEventTime :: Widget t m => EventTime -> m ()
printEventTime (SimpleEvent start end) = do
   let printFullTime time = toText (formatTime defaultTimeLocale "%a %Y-%m-%d %H:%M " (time ^. #time)) <> time ^. #zone
   let printEndTime = toText . formatTime defaultTimeLocale "%H:%M" . (^. #time)
   D.text (printFullTime start)
   icon "" "arrow_right_alt"
   D.text (printEndTime end)
printEventTime (AllDayEvent startDay endDay) = do
   let printFullTime = toText . formatTime defaultTimeLocale "%a %Y-%m-%d"
   D.text (printFullTime startDay)
   when (startDay /= endDay) do
     icon "" "arrow_right_alt"
     D.text (printFullTime endDay)
printEventTime _ = pass
