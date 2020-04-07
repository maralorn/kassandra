{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances, OverloadedLabels, TypeApplications, AllowAmbiguousTypes, ScopedTypeVariables, StandaloneDeriving, DuplicateRecordFields #-}
module Frontend.Types
  ( Widget
  , ViewWidget
  , WidgetIO
  , TaskInfos(..)
  , TaskState
  , AppStateChange
  , DragState
  , TaskTreeStateChange
  , DataChange
  , FilterState
  , StandardWidget
  , AppState(AppState)
  , getAppState
  , getTasks
  , getDragState
  , getFilterState
  , getTime
  , fl
  , al
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Taskwarrior.Task
import qualified Taskwarrior.Status
import qualified Data.Aeson                    as Aeson
import           Data.Generics.Sum.Typed        ( AsType )

type Widget t m
  = ( D.DomBuilder t m
    , D.DomBuilderSpace m ~ D.GhcjsDomSpace
    , MonadFix m
    , R.MonadHold t m
    , R.PostBuild t m
    )
type WidgetIO t m
  = ( MonadIO m
    , Widget t m
    , R.TriggerEvent t m
    , R.PerformEvent t m
    , MonadIO (R.Performable m)
    )
data TaskInfos = TaskInfos { task :: Task, children :: [UUID], parents :: [UUID], revDepends :: [UUID], blocked :: Bool} deriving (Eq, Show, Generic)
makeLabels ''TaskInfos

type TaskState = (HashMap UUID TaskInfos)

data DragState = DraggedTask UUID | DraggedTag Text | NoDrag deriving (Eq, Show, Generic)
makePrismLabels ''DragState

data DataChange = ChangeTask Task | CreateTask Text (Task -> Task) deriving (Generic)
makePrismLabels ''DataChange

data ToggleEvent = ToggleEvent UUID Bool deriving (Eq, Show, Generic)
makePrismLabels ''ToggleEvent

type AppStateChange = Either DragState DataChange
type TaskTreeStateChange = Either AppStateChange ToggleEvent

data FilterState = FilterState { deletedFade :: NominalDiffTime,  completedFade :: NominalDiffTime}deriving (Eq, Show, Generic)
makeLabels ''FilterState

data AppState t = AppState { taskState :: R.Dynamic t TaskState, currentTime :: R.Dynamic t ZonedTime , dragState :: R.Dynamic t DragState, filterState :: R.Dynamic t FilterState} deriving (Generic)
makeLabels ''AppState

type ViewWidget t m r e
  = ( Widget t m
    , R.EventWriter t (NonEmpty e) m
    , MonadReader r m
    , HasType (AppState t) r
    )

type StandardWidget t m r e = (AsType AppStateChange e, ViewWidget t m r e)
type TaskTreeWidget t m r e = (AsType TaskTreeStateChange e, ViewWidget t m r e)

getAppState :: (MonadReader r m, HasType (AppState t) r) => m (AppState t)
getAppState = asks (^. typed)
getTasks
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t TaskState)
getTasks = getAppState ^. al #taskState
getDragState
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t DragState)
getDragState = getAppState ^. al #dragState
getFilterState
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t FilterState)
getFilterState = getAppState ^. al #filterState
getTime
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t ZonedTime)
getTime = getAppState ^. al #currentTime

fl :: (Functor f, Is k A_Getter) => Optic' k is s a -> Getter (f s) (f a)
fl a = to (view a <$>)
al
  :: (Applicative a, Is k A_Setter, Is k A_Getter)
  => Optic' k is s b
  -> Lens' (a s) (a b)
al a = lens (view a <$>) (\s b -> liftA2 (set a) b s)

deriving instance Generic Task
makeLabels ''Task
deriving instance Generic Status
makeLabels ''Status
makePrismLabels ''Status
deriving instance Generic (Aeson.Result a)
makePrismLabels ''Aeson.Result


instance LabelOptic "partof" A_Lens Task Task (Maybe UUID) (Maybe UUID) where
  labelOptic = lens
    ((^. #uda % at "partof") >=> (^? #_Success) . fromJSON)
    (\task uuid -> #uda % at "partof" .~ (toJSON <$> uuid) $ task)
instance LabelOptic "description" A_Lens TaskInfos TaskInfos Text Text where
  labelOptic = (#task :: Lens' TaskInfos Task) % #description
instance LabelOptic "uuid" A_Lens TaskInfos TaskInfos UUID UUID where
  labelOptic = #task % #uuid
instance LabelOptic "status" A_Lens TaskInfos TaskInfos Status Status where
  labelOptic = #task % #status
instance LabelOptic "tags" A_Lens TaskInfos TaskInfos [Text] [Text] where
  labelOptic = #task % #tags
instance LabelOptic "partof" A_Lens TaskInfos TaskInfos (Maybe UUID) (Maybe UUID) where
  labelOptic = #task % #partof
instance LabelOptic "modified" A_Lens TaskInfos TaskInfos (Maybe UTCTime) (Maybe UTCTime) where
  labelOptic = #task % #modified
instance LabelOptic "depends" A_Lens TaskInfos TaskInfos [UUID] [UUID] where
  labelOptic = #task % #depends
instance Field1 s t a b =>LabelOptic "_1" A_Lens s t a b where
  labelOptic = _1
instance Field2 s t a b =>LabelOptic "_2" A_Lens s t a b where
  labelOptic = _2
instance (Functor f, Is k A_Getter,LabelOptic "status" k s s a a, c ~ f a, d ~ f a) => LabelOptic "status" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"status"
instance (Functor f, Is k A_Getter,LabelOptic "showChildren" k s s a a, c ~ f a, d ~ f a) => LabelOptic "showChildren" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"showChildren"
instance (Functor f, Is k A_Getter,LabelOptic "uuid" k s s a a, c ~ f a, d ~ f a) => LabelOptic "uuid" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"uuid"
instance (Functor f, Is k A_Getter,LabelOptic "description" k s s a a, c ~ f a, d ~ f a) => LabelOptic "description" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"description"
instance (Functor f, Is k A_Getter,LabelOptic "partof" k s s a a, c ~ f a, d ~ f a) => LabelOptic "partof" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"partof"
instance (Functor f, Is k A_Getter,LabelOptic "tags" k s s a a, c ~ f a, d ~ f a) => LabelOptic "tags" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"tags"
instance (Functor f, Is k A_Getter,LabelOptic "depends" k s s a a, c ~ f a, d ~ f a) => LabelOptic "depends" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"depends"

instance (R.Reflex t, c ~ R.Behavior t a) => LabelOptic "current" A_Getter (R.Dynamic t a) (R.Dynamic t a) c c where
  labelOptic = to R.current
