
module Frontend.TimeWidgets
  ( 
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Frontend.Types                 ( Widget
                                                , StandardWidget
                                                , getTime
                                                )
import           Frontend.TextEditWidget        ( editText )
import           Frontend.BaseWidgets           ( stateWidget
                                                , button
                                                , icon
                                                )
