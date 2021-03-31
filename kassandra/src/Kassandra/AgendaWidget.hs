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
      calendarEvents <&> mapM_ \CalendarEvent{description, time, calendarName} -> do
        D.divClass "event" $ do
          icon "" "description"
          D.text description
          D.el "br" pass
          icon "" "event"
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
printEventTime _ = pass
