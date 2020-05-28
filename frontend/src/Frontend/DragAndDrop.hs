{-# LANGUAGE PatternSynonyms #-}


module Frontend.DragAndDrop
  ( 
  )
where

import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Data.Proxy                     ( Proxy(Proxy) )
import           Frontend.Sorting               ( SortPosition
                                                , saveSorting
                                                )
import           Frontend.Types                 ( AppStateChange
                                                , StandardWidget
                                                , TaskInfos
                                                , DragState(DraggedTask, NoDrag)
                                                , getTasks
                                                , getDragState
                                                , DataChange
                                                , WriteApp
                                                )
import           Frontend.Util                  ( tellSingleton
                                                , lookupTask
                                                )
