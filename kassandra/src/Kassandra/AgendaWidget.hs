{-# LANGUAGE BlockArguments #-}

module Kassandra.AgendaWidget (agendaWidget) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Kassandra.BaseWidgets
import Kassandra.Calendar
import Kassandra.Config (DefinitionElement (..), ListItem (..))
import Kassandra.TextEditWidget (createTextWidget)
import Kassandra.Types
import Kassandra.Util
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
    ( (>> D.el "br" pass)
        . ( \case
              ConfigList _ _ -> error "ConfigLists are not implemented"
              ListElement el -> case el of
                (TaskwarriorTask _) -> error "Can‘t display tw tasks"
                (AdHocTask t) -> do
                  changeDoneStatus <- if t `Set.member` completed calendarList then (False <$) <$> button "" (D.text "[x]") else (True <$) <$> button "" (D.text "[ ]")
                  D.text [i| #{t}|]
                  delete <- button "" (D.text "x")
                  tellList $ delete $> (#entries %~ Seq.filter (ListElement (AdHocTask t) /=)) calendarList
                  tellList $
                    changeDoneStatus <&> \case
                      True -> (#completed %~ Set.insert t) calendarList
                      False -> (#completed %~ Set.delete t) calendarList
                (HabiticaTask _) -> error "HabiticaTasks are not yet supported"
                (Mail _) -> error "Mails are not yet supported"
          )
    )
  newTaskEvent <-
    createTextWidget
      (button "selector" $ D.text "New Task")
  tellList $ newTaskEvent <&> \content -> (#entries %~ (Seq.|> ListElement (AdHocTask content))) calendarList
 where
  tellList listEvent = tellSingleton $ (_Typed @AppStateChange % _Typed @DataChange #) . SetEventList uid <$> listEvent
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
