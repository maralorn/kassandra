{-# LANGUAGE TypeApplications, TupleSections, FlexibleContexts, ConstraintKinds, StandaloneDeriving, NamedFieldPuns, LambdaCase, RecursiveDo, QuasiQuotes, ScopedTypeVariables, GADTs, TemplateHaskell, OverloadedLabels, ViewPatterns #-}
module Frontend.State
  ( stateProvider
  , TaskProvider
  , CacheProvider
  , StateProvider
  , Cache(Cache, collapseState)
  )
where

import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import qualified Data.Dependent.Map            as DMap
import qualified Data.GADT.Compare.TH          as TH
import           Frontend.Types

data FanTag a where
   ToggleEventTag ::FanTag (NonEmpty (UUID, Bool))
   ChangeTaskTag ::FanTag (NonEmpty Task)
   CreateTaskTag ::FanTag (NonEmpty (Text, Task -> Task))

TH.deriveGEq ''FanTag
TH.deriveGCompare ''FanTag

newtype Cache = Cache { collapseState :: HashMap UUID Bool } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

type CacheProvider t m
  = R.Event t (NonEmpty (UUID, Bool)) -> m (R.Dynamic t Cache)

type TaskProvider t m
  =  R.Event t (NonEmpty Task)
  -> R.Event t (NonEmpty (Text, Task -> Task))
  -> m (R.Dynamic t (HashMap UUID Task))

getParents :: HashMap UUID Task -> UUID -> [UUID]
getParents tasks = go [] (\uuid -> (^. #partof) =<< tasks ^. at uuid)
 where
  go :: (Eq a, Show a) => [a] -> (a -> Maybe a) -> a -> [a]
  go accu f x | x `elem` accu    = []
              | Just next <- f x = next : go (x : accu) f next
              | otherwise        = []


type StateProvider t m
  = R.Event t (NonEmpty DataChange) -> m (R.Dynamic t TaskState)

stateProvider
  :: forall t m
   . (WidgetIO t m)
  => CacheProvider t m
  -> TaskProvider t m
  -> StateProvider t m
stateProvider cacheProvider taskProvider stateChange = do
  cache <- cacheProvider $ R.select fannedEvent ToggleEventTag
  tasks <- taskProvider (R.select fannedEvent ChangeTaskTag)
                        (R.select fannedEvent CreateTaskTag)
  pure $ R.zipDynWith buildTaskInfosMap tasks cache
 where
  mapToMap = \case
    ToggleEvent a b -> DMap.singleton ToggleEventTag (Identity $ pure (a, b))
    ChangeTask a    -> DMap.singleton ChangeTaskTag (Identity $ pure a)
    CreateTask a b  -> DMap.singleton CreateTaskTag (Identity $ pure (a, b))
  proofAdd :: FanTag a -> Identity a -> Identity a -> Identity a
  proofAdd = liftA2 . \case
    ToggleEventTag -> (<>)
    ChangeTaskTag  -> (<>)
    CreateTaskTag  -> (<>)
  fannedEvent =
    R.fan
      $   DMap.unionsWithKey proofAdd
      .   fmap mapToMap
      .   NonEmpty.toList
      <$> stateChange
  buildChildrenMap :: HashMap a Task -> HashMap UUID [a]
  buildChildrenMap =
    HashMap.fromListWith (++)
      . mapMaybe (\(uuid, task) -> (, pure uuid) <$> task ^. #partof)
      . HashMap.toList
  buildTaskInfosMap tasks cache = HashMap.mapWithKey
    (\u t -> TaskInfos t
                       (HashMap.lookupDefault False u (collapseState cache))
                       (HashMap.lookupDefault [] u childrenMap)
                       (getParents tasks u)
    )
    tasks
    where childrenMap = buildChildrenMap tasks
