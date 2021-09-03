module Kassandra.ListWidget (
  listsWidget,
  listWidget,
) where

import qualified Data.HashMap.Strict as HashMap
import Kassandra.Config (DefinitionElement (TagList))
import Kassandra.ListElementWidget (AdhocContext (NoContext), definitionElementWidget)
import Kassandra.Types (
  StandardWidget,
  TaskState,
  Widget,
  getTasks,
 )
import qualified Reflex as R
import qualified Reflex.Dom as D

listsWidget :: (StandardWidget t m r e) => m ()
listsWidget = do
  taskState <- getTasks
  D.text "Select a list"
  list <- listSelector (getLists <$> taskState)
  maybeList <- R.maybeDyn list
  D.dyn_ $ maybeList <&> maybe (D.text "Select a list") listWidget
 where
  getLists :: TaskState -> Seq Text
  getLists =
    fromList
      . toList
      . foldMap (^. #tags)
      . filter (has $ #status % #_Pending)
      . (^. mapping #task)
      . HashMap.elems
  listSelector ::
    (Widget t m) => R.Dynamic t (Seq Text) -> m (R.Dynamic t (Maybe Text))
  listSelector lists = D.el "div" $ do
    buttons <- D.dyn $ mapM listButton <$> lists
    buttonSum <- R.switchHold R.never $ R.leftmost . toList <$> buttons
    R.holdDyn Nothing (Just <$> buttonSum)
  listButton :: Widget t m => Text -> m (R.Event t Text)
  listButton tag =
    fmap ((tag <$) . D.domEvent D.Click . fst)
      . D.elClass' "a" "selector"
      . D.text
      $ tag

listWidget ::
  forall t m r e. StandardWidget t m r e => R.Dynamic t Text -> m ()
listWidget list = D.dyn_ (innerRenderList <$> list)
 where
  innerRenderList :: Text -> m ()
  innerRenderList tag = definitionElementWidget NoContext (TagList tag)
