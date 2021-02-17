{-# LANGUAGE BlockArguments #-}

module Kassandra.AgendaWidget (agendaWidget) where

import Kassandra.Calendar
import Kassandra.Types
import qualified Reflex.Dom as D
import Kassandra.BaseWidgets
import Data.Time

agendaWidget :: StandardWidget t m r e => m ()
agendaWidget = do
  appState <- getAppState
  let calendarEvents = appState ^. #calendarEvents
  void $
    D.dyn_ $
      calendarEvents <&> mapM_ \CalendarEvent{description, time} -> do
        D.divClass "event" $ do
          icon "" "description"
          D.text description
          D.el "br" pass
          icon "" "event"
          printEventTime time

printEventTime :: Widget t m => EventTime -> m ()
printEventTime (SimpleEvent start end) = do
   timeZone <- liftIO getCurrentTimeZone
   let printFullTime = toText . formatTime defaultTimeLocale "%A %H:%M" . utcToZonedTime timeZone
   let printEndTime = toText . formatTime defaultTimeLocale "%H:%M" . utcToZonedTime timeZone
   D.text (printFullTime start)
   icon "" "arrow_right_alt"
   D.text (printEndTime end)
printEventTime _ = pass
