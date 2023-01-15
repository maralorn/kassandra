{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kassandra.ReflexUtil (
  smartSimpleList,
  listWithGaps,
  keyDynamic,
) where

import qualified Data.Map as Map
import qualified Data.Patch.Map as Patch
--import qualified Data.Patch.MapWithMove as Patch
import qualified Data.Sequence as Seq
import qualified Reflex as R
import qualified Reflex.Dom as D

{- | Renders a list of widgets depending on a Dynamic list of inputs. This will
 call the widget constructor once per value in the list.
 When the list changes, the widget will move and reuse all values that it can
 so that it only needs to call the constructor again, when a new value (or a
 second copy of a same value) appears in the list.
-}
smartSimpleList ::
  forall t m v.
  (R.Adjustable t m, R.PostBuild t m, Ord v, R.MonadHold t m, MonadFix m, D.NotReady t m) =>
  (v -> m ()) ->
  R.Dynamic t (Seq v) ->
  m ()
smartSimpleList widget listElements = do
  void $ R.simpleList (toList <$> listElements) \vDyn -> do
     u <- R.holdUniqDyn vDyn
     D.dyn_ . fmap widget $ u
  --postBuild <- R.getPostBuild
  --keyMap <- R.holdUniqDyn $ Seq.foldMapWithIndex (curry one) <$> listElements
  --let keyMapChange =
        --R.attachWith
          --((Newtype.under @(Map Int (Patch.NodeInfo Int v)) fixPatchMap .) . Patch.patchThatChangesMap)
          --(R.current keyMap)
          --(R.updated keyMap)
      --initialKeyMap = Patch.patchMapWithMoveInsertAll <$> R.tag (R.current keyMap) postBuild
      --keyMapEvents = keyMapChange <> initialKeyMap
  --void $ R.mapMapWithAdjustWithMove (const widget) mempty keyMapEvents

-- | A workaround for a bug in patchThatChangesMap in patch 0.0.3.2.
--fixPatchMap :: Map Int (Patch.NodeInfo Int v) -> Map Int (Patch.NodeInfo Int v)
--fixPatchMap inputMap = appEndo setMoves . fmap (Patch.nodeInfoSetTo Nothing) $ inputMap
-- where
--  setMoves = Map.foldMapWithKey f inputMap
--  f to' (Patch.NodeInfo (Patch.From_Move from) _) = Endo $ Map.adjust (Patch.nodeInfoSetTo (Just to')) from
--  f _ _ = mempty

listWithGaps ::
  (R.Adjustable t m, R.PostBuild t m, R.MonadHold t m, MonadFix m, Ord v, D.NotReady t m) =>
  (v -> m ()) ->
  (R.Dynamic t (Maybe v, Maybe v) -> m ()) ->
  R.Dynamic t (Seq v) ->
  m ()
listWithGaps widget gapWidget listD = do
  smartSimpleList elementWidget listD
  lastElementD <- R.holdUniqDyn $ (,Nothing) . lastOf folded <$> listD
  gapWidget lastElementD
 where
  elementWidget currentElement = do
      elementPair <- R.holdUniqDyn $ (,Just currentElement) . Map.lookup currentElement <$> prevElementsD
      gapWidget elementPair
      widget currentElement
  prevElementsD = (\xs -> Map.unions . fmap one $ Seq.zip (Seq.drop 1 xs) xs) <$> listD

keyDynamic ::
  forall t k v.
  (R.Reflex t, Ord k) =>
  R.Incremental t (Patch.PatchMap k v) ->
  k ->
  R.Dynamic t (Maybe v)
keyDynamic incremental key =
  R.incrementalToDynamic
    . R.unsafeMapIncremental mapMap mapPatchMap
    $ incremental
 where
  mapMap :: Map k v -> Maybe v
  mapMap = Map.lookup key
  mapPatchMap :: Patch.PatchMap k v -> Identity (Maybe v)
  mapPatchMap = Identity . join . Map.lookup key . Patch.unPatchMap
