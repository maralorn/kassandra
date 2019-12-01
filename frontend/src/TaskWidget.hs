{-# LANGUAGE TypeApplications, TupleSections, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables #-}
module TaskWidget
  ( taskWidget
  )
where
import           ClassyPrelude
import qualified Data.HashMap.Strict           as HashMap
import           Data.List.NonEmpty             ( NonEmpty
                                                , nonEmpty
                                                )
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as D
import qualified Data.Maybe                    as Maybe
import qualified Reflex                        as R
import qualified Taskwarrior.Task              as Task
import qualified Taskwarrior.Status            as Status
import           Taskwarrior.Status             ( Status )
import           Types
import           TextEditWidget
import           Data.Time.LocalTime            ( zonedTimeToUTC )

taskWidget
  :: forall m t
   . (ViewWidget t m (NonEmpty StateChange))
  => R.Dynamic t TaskInfos
  -> m ()
taskWidget taskInfos' = D.divClass "task" $ do
  taskInfos <- R.holdUniqDyn taskInfos'
  let taskDyn = task <$> taskInfos
  statusEventFromWidget <-
    fmap snd . D.runEventWriterT . statusWidget $ Task.status <$> taskDyn
  let showChilds = showChildren <$> taskInfos
  taskState <- getTaskState <$> ask
  children  <-
    R.holdUniqDyn
    $   (Maybe.mapMaybe <$> flip HashMap.lookup)
    <$> taskState
    <*> (children <$> taskInfos)
  D.dyn
      ((\s -> if not $ null s then collapseButton showChilds else pure R.never)
      <$> children
      )
    >>= (D.attachPromptlyDynWith
          (((singleton . ToggleEvent) .) . ((,) . Task.uuid . task))
          taskInfos <$>
        )
    .   R.switchHold R.never
    >>= R.tellEvent
  newDescriptionEvent <- lineWidget $ Task.description <$> taskDyn
  D.tellEvent $ R.attachPromptlyDynWith
    (\t d -> singleton $ ChangeTask t { Task.description = d })
    taskDyn
    newDescriptionEvent
  statusEventFromDelete <-
    fmap snd . D.runEventWriterT . deleteButton $ Task.status <$> taskDyn
  waitTimeEvent <-
    fmap snd . D.runEventWriterT . dateSelectionWidget "wait" $ R.constDyn
      Nothing :: m (R.Event t (NonEmpty (Maybe UTCTime)))
  let statusEventFromWait =
        fmap Status.Waiting
          <$> R.fmapMaybe (nonEmpty . catMaybes . toList) waitTimeEvent
  R.tellEvent
    . R.attachPromptlyDynWith
        (\task -> fmap (\status -> ChangeTask task { Task.status }))
        taskDyn
    $ (statusEventFromDelete ++ statusEventFromWidget ++ statusEventFromWait)
  D.dyn_
    $   (\s -> if s
          then D.divClass "children" $ R.simpleList children taskWidget
          else pure (R.constDyn [])
        )
    <$> showChilds


deleteButton :: ViewWidget t m (NonEmpty Status) => R.Dynamic t Status -> m ()
deleteButton statusDyn =
  D.dyn_
    $   (\case
          (Status.Deleted _) -> D.blank
          _                  -> do
            time  <- fmap zonedTimeToUTC . getTime <$> ask
            event <- D.tagPromptlyDyn time <$> iconButtonClass "delete" "edit"
            R.tellEvent $ singleton . Status.Deleted <$> event
        )
    <$> statusDyn

statusWidget
  :: forall t m . ViewWidget t m (NonEmpty Status) => R.Dynamic t Status -> m ()
statusWidget status = do
  D.dyn_ $ dynWidget . widgetState <$> status
 where
  dynWidget
    :: (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status)) -> m ()
  dynWidget (iconName, showClass, handlerMay) = do
    (el, ()) <- D.elAttr' "div" ("class" =: "checkbox") $ do
      D.elClass
          "i"
          ("material-icons " ++ showClass ++ if isJust handlerMay
            then " hideable"
            else ""
          )
        $ D.text iconName
      for_ handlerMay $ \(altIcon, altClass, _) -> do
        D.elClass "i" ("material-icons " ++ altClass ++ " showable")
          $ D.text altIcon
    for_ handlerMay $ \(_, _, handler) -> do
      time <- getTime <$> ask
      R.tellEvent
        . fmap (singleton . handler . zonedTimeToUTC)
        . D.tagPromptlyDyn time
        . D.domEvent D.Mouseup
        $ el
  widgetState
    :: Status -> (Text, Text, Maybe (Text, Text, UTCTime -> Status.Status))
  widgetState = \case
    Status.Pending ->
      ("done", "hide", Just ("done", "grey", Status.Completed))
    (Status.Completed _) ->
      ("done", "show", Just ("done", "hide", const Status.Pending))
    (Status.Deleted _) ->
      ("delete", "show", Just ("done", "hide", const Status.Pending))
    (Status.Waiting _) ->
      ("schedule", "show", Just ("done", "grey", Status.Completed))
    (Status.RecurringParent _ _ ) -> ("repeat", "show", Nothing)
    (Status.RecurringChild _ _ _) -> ("repeat", "show", Nothing)


collapseButton :: (Widget t m) => R.Dynamic t Bool -> m (R.Event t Bool)
collapseButton open = do
  let label = (\s -> if s then "unfold_less" else "unfold_more")
  buttonEventEvent <- D.dyn $ flip iconButtonClass "collapse" . label <$> open
  toggleShow       <- R.switchHold R.never buttonEventEvent
  pure $ R.tagPromptlyDyn (not <$> open) toggleShow
