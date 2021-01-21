module Kassandra.Sorting
  ( sortTasks
  , saveSorting
  , SortPosition(SortPosition)
  , SortMode(SortModePartof, SortModeTag)
  ) where

import qualified Data.Aeson                    as Aeson
import           Data.Scientific                ( toRealFloat )
import           Data.Set                       ( member )
import           Kassandra.Types                ( TaskInfos )
import qualified Reflex                        as R
import           Relude.Extra.Foldable1         ( maximum1 )
import qualified Taskwarrior.Task              as Task

data SortMode = SortModePartof UUID | SortModeTag Task.Tag
   deriving stock (Show, Eq, Ord, Generic)
makePrismLabels ''SortMode

data SortState = HasSortPos Double | WillWrite { iprev :: Double, dprev :: Int, inext :: Double, dnext :: Int }
   deriving stock (Eq, Show, Ord, Read, Generic)
makePrismLabels ''SortState

declareFieldLabels [d|data SortPosition t = SortPosition {
    mode :: R.Behavior t SortMode,
    list :: R.Behavior t [Task],
    before :: R.Behavior t (Maybe UUID)
}|]

sortTasks :: SortMode -> [TaskInfos] -> [TaskInfos]
sortTasks mode = sortOn (getSortOrder mode . (^. #task))

getSortOrder :: SortMode -> Task -> Maybe Double
getSortOrder mode = valToNumber <=< (^. #uda % at (sortFieldName mode))

valToNumber :: Aeson.Value -> Maybe Double
valToNumber = \case
  Aeson.Number a -> _Just # toRealFloat a
  Aeson.String a -> readMaybe $ a ^. unpacked
  _              -> Nothing

sortFieldName :: SortMode -> Text
sortFieldName = \case
  SortModeTag    tag -> "kassandra_tag_pos_" <> tag
  SortModePartof _   -> "kassandra_partof_pos"

setSortOrder :: SortMode -> Double -> Task -> Task
setSortOrder mode val = #uda %~ at (sortFieldName mode) ?~ Aeson.toJSON val

taskInList :: SortMode -> Task -> Bool
taskInList (SortModePartof uuid) = (Just uuid ==) . (^. #partof)
taskInList (SortModeTag    tag ) = member tag . Task.tags

insertInList :: SortMode -> Task -> Task
insertInList (SortModePartof uuid) = #partof ?~ uuid
insertInList (SortModeTag    tag ) = #tags %~ addTag
 where
  addTag tags | tag `member` tags = tags
              | otherwise         = tags <> one tag

unSetSortOrder :: SortMode -> Task -> Task
unSetSortOrder mode = #uda %~ sans (sortFieldName mode)

-- ! Returns a list of tasks for which the UDA attributes need to be changed to reflect the order of the given list.
sortingChanges :: SortMode -> [Task] -> [Task]
sortingChanges mode list =
  let
    addState = addSortState (getSortOrder mode)
    assureSort delta = applyUntil
      (addState . unSetWorstUnsorted (unSetSortOrder mode) delta)
      (tasksSorted delta)
    sortedList = assureSort 0 $ addState list
    finalList | tasksSorted minDist sortedList = sortedList
              | otherwise = assureSort minTouchedDist sortedList
    getWrite (task, sortState)
      | has #_WillWrite sortState || not (taskInList mode task)
      = Just . setSortOrder mode (newValue sortState) . insertInList mode $ task
      | otherwise
      = Nothing
  in
    mapMaybe getWrite finalList

applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil f condition x | condition x = x
                         | otherwise   = applyUntil f condition (f x)

minOrder, maxOrder, minDist, minTouchedDist :: Double
minOrder = -1
maxOrder = -minOrder
minDist = 10 ** (-6)
minTouchedDist = 10 ** (-3)


tasksSorted :: Show a => Double -> [(a, SortState)] -> Bool
tasksSorted = isSortedOn (newValue . (^. _2))
isSortedOn :: Show a => (a -> Double) -> Double -> [a] -> Bool
isSortedOn f delta = \case
  []           -> True
  [_         ] -> True
  (x : y : ys) -> f x + delta < f y && isSortedOn f delta (y : ys)

unSetWorstUnsorted :: (a -> a) -> Double -> [(a, SortState)] -> [a]
unSetWorstUnsorted _ _ [] = []
unSetWorstUnsorted unSet delta (x : xs)
  | ((fst <$>) -> fine, (fst <$>) -> a : alsoFine) <- break ((worst ==) . snd)
                                                            (toList badnesses)
  = fine <> (unSet a : alsoFine)
  | otherwise
  = error "Assumed wrong invariant in unSetWorstUnsorted" -- The list of badnesses has to contain its maximum
 where
  badnesses = go [] (x :| xs) <&> \(a, _, badness) -> (a, badness)
  worst     = maximum1 $ snd <$> badnesses
  go :: [Double] -> NonEmpty (a, SortState) -> NonEmpty (a, Double, Int)
  go before ((a, s@(newValue -> value)) :| ys)
    | has #_WillWrite s = (a, value, 0) :| rest
    | otherwise         = (a, value, foundBefore + foundAfter) :| rest
   where
    foundBefore        = length $ filter (value - delta <=) before
    (foundAfter, rest) = maybe (0, []) countThroughRest (nonEmpty ys)
    countThroughRest list =
      (length $ filter (\(_, int, _) -> value + delta >= int) zs, zs)
      where zs = toList $ go (value : before) list

newValue :: SortState -> Double
newValue (HasSortPos x) = x
newValue (WillWrite iprev (fromIntegral -> dprev) inext (fromIntegral -> dnext))
  | dprev + dnext == 0 = iprev
  | otherwise          = iprev + ((inext - iprev) * dprev / (dprev + dnext))

sortStateNext :: SortState -> (Double, Int)
sortStateNext (HasSortPos a       ) = (a, 0)
sortStateNext (WillWrite _ _ int d) = (int, d)

addSortState :: forall a . (a -> Maybe Double) -> [a] -> [(a, SortState)]
addSortState f = go (minOrder, 0)
 where
  go :: (Double, Int) -> [a] -> [(a, SortState)]
  go (iprev, dprev) list
    | [] <- list
    = []
    | (x : xs) <- list
    , Just int <- f x
    = (x, HasSortPos int) : go (int, 0) xs
    | (x : xs) <- list
    , next@((_, sortStateNext -> (inext, dnext)) : _) <- go (iprev, dprev + 1)
                                                            xs
    = (x, WillWrite iprev (dprev + 1) inext (dnext + 1)) : next
    | (x : _) <- list
    = one (x, WillWrite iprev (dprev + 1) maxOrder 1)

insertBefore :: [Task] -> [Task] -> Maybe UUID -> [Task]
insertBefore list toInsert = \case
  Just uuid ->
    let (front, back) = break ((== uuid) . (^. #uuid)) cleanList
    in  front <> (toInsert ++ back)
  Nothing -> cleanList <> toInsert
  where cleanList = filter ((`notElem` (toInsert ^. #uuid)) . (^. #uuid)) list

type InsertEvent t = R.Event t (NonEmpty Task, Maybe UUID)

saveSorting
  :: R.Reflex t
  => R.Behavior t SortMode
  -> R.Behavior t [Task]
  -> InsertEvent t
  -> R.Event t (NonEmpty Task)
saveSorting modeB listB = R.fmapMaybe nonEmpty
  . R.attachWith attach ((,) <$> modeB <*> listB)
 where
  attach (mode, list) (toList -> tasks, before) =
    sortingChanges mode $ insertBefore list tasks before
