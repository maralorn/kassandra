{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kassandra.ReflexUtil
  ( smartSimpleList
  , keyDynamic
  )
where

import qualified Reflex                        as R
import qualified Data.Patch.MapWithMove        as Patch
import qualified Data.Patch.Map                as Patch
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )

-- | Renders a list of widgets depending on a Dynamic list of inputs. This will
-- call the widget constructor once per value in the list.
-- When the list changes, the widget will move and reuse all values that it can
-- so that it only needs to call the constructor again, when a new value (or a
-- second copy of a same value) appears in the list.
smartSimpleList
  :: (R.Adjustable t m, R.PostBuild t m, Ord v)
  => (v -> m ())
  -> R.Dynamic t (Seq v)
  -> m ()
smartSimpleList widget listElements = do
  postBuild <- R.getPostBuild
  let keyMap       = Seq.foldMapWithIndex Map.singleton <$> listElements
      keyMapChange = R.attachWith Patch.patchThatChangesMap
                                  (R.current keyMap)
                                  (R.updated keyMap)
      initialKeyMap =
        Patch.patchMapWithMoveInsertAll <$> R.tag (R.current keyMap) postBuild
      keyMapEvents = keyMapChange <> initialKeyMap
  void $ R.mapMapWithAdjustWithMove (const (widget)) mempty keyMapEvents

keyDynamic
  :: forall t k v
   . (R.Reflex t, Ord k)
  => R.Incremental t (Patch.PatchMap k v)
  -> k
  -> R.Dynamic t (Maybe v)
keyDynamic incremental key =
  R.incrementalToDynamic
    . R.unsafeMapIncremental mapMap mapPatchMap
    $ incremental
 where
  mapMap :: Map k v -> Maybe v
  mapMap = Map.lookup key
  mapPatchMap :: Patch.PatchMap k v -> Identity (Maybe v)
  mapPatchMap = Identity . join . Map.lookup key . Patch.unPatchMap
