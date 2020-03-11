module Frontend.Css
  ( css
  )
where

import           Prelude                 hiding ( (&)
                                                , i
                                                )
import           Clay


css :: Text
css = toStrict . render $ do
  star ? do
    fontFamily ["B612"] []
    padding (px 0) (px 0) (px 0) (px 0)
    margin (px 0) (px 0) (px 0) (px 0)
  body ? do
    background (rgb 0 0 33)
    color white
  ".container" ? do
    display flex
  ".dropHere" ? do
    position absolute
    color black
    background (rgb 255 200 20)
  ".above" ? marginTop (em (-0.8))
  ".tag" ? do
    background (rgb 200 200 255)
    borderRadius (em 0.1) (em 0.1) (em 0.1) (em 0.1)
    padding (em 0.1) (em 0.1) (em 0.1) (em 0.1)
    margin (em 0.1) (em 0.1) (em 0.1) (em 0.1)
  ".material-icons" ? do
    fontFamily ["Material Icons"] []
    fontWeight normal
    fontStyle normal
    fontSize (em 1)
    display inlineBlock
    lineHeight (em 1)
    width (em 1)
    textTransform none
    letterSpacing normal
    wordWrap normal
    whiteSpace nowrap
    direction ltr
    "-webkit-font-smoothing" -: "antialiased"
  let radius    = em 0.3
      bg        = grayish 230
      blockSize = do
        width (em 1)
        height (em 1)
      taskMargin = em 0.5
  ".task" ? do
    color (rgb 0 0 33)
    background white
    margin taskMargin taskMargin taskMargin taskMargin
    padding taskMargin taskMargin taskMargin taskMargin
    ".task" ? do
      margin (px 0) (px 0) (px 0) (px 0)
      padding (em 0.2) (em 0.2) (em 0.2) (em 0.2)
    hover & border solid (px 1) blue
    border solid (px 1) white
    ".edit" <? visibility hidden
    hover & ".edit" <? visibility visible

  ".button" ? do
    hover & color blue
  i ? cursor cursorDefault
  ".icon" ? padding radius radius radius radius
  ".checkbox" ? do
    marginRight (em 0.5)
    display inlineBlock
    blockSize
    padding radius radius radius radius
    borderRadius radius radius radius radius
    background bg
    i ? do
      ".hide" & color bg
      ".grey" & color (grayish 160)
      ".show" & color black
      ".showable" & display none
    hover & i ? do
      ".hideable" & display none
      ".showable" & display inlineBlock
  ".children" ? paddingLeft (px 20)
