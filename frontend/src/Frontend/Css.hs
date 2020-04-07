module Frontend.Css
  ( css
  )
where

import           Prelude                 hiding ( (&)
                                                , (|>)
                                                , i
                                                )
import           Clay


css :: Text
css = toStrict . render $ do
  let darkBlue      = rgb 0 0 33
      veryLightBlue = rgb 235 235 255
      lightBlue     = rgb 200 200 255
      lighterBlue   = rgb 144 144 255
      sunYellow     = rgb 255 200 20
      noMargin      = margin (px 0) (px 0) (px 0) (px 0)
      noPadding     = padding (px 0) (px 0) (px 0) (px 0)
  star ? do
    fontFamily ["B612"] []
    noMargin
    noPadding
  body ? do
    background darkBlue
    color white
  ".container" ? display flex
  ".dropHere" ? do
    position absolute
    color black
    background sunYellow
  let offset = 1.5
  ".plusOne" ? marginLeft (em offset)
  ".plusTwo" ? marginLeft (em (offset * 2))
  ".above" ? marginTop (em (-0.8))
  ".pane" ? width (pct 100)
  ".selector" ? do
    display inlineBlock
    margin (px 1) (px 1) (px 1) (px 1)
    color (rgb 0 0 200)
    padding (em 0.1) (em 0.3) (em 0.1) (em 0.3)
    background lightBlue
  let tagRadius = em 0.4
  ".tag" ? ".icon" ? do
    position absolute
    borderRadius tagRadius tagRadius tagRadius tagRadius
    background lightBlue
    marginLeft (em (-1.1))
    marginTop (em 0.70)
    fontSize (em 0.85)
  ".tag" ? do
    background (rgb 200 200 255)
    borderRadius tagRadius tagRadius tagRadius tagRadius
    padding (em 0.1) (em 0.3) (em 0.1) (em 0.3)
    margin (em 0.1) (em 0.1) (em 0.1) (em 0.1)
    fontSize (em 0.8)
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
  ".path" ? do
    color grey
    fontSize (em 0.8)
  let radius       = em 0.3
      leftBarWidth = em 1.8
  ".activeEdit" ? do
    border solid (px 1) sunYellow
    background sunYellow
    padding (px 1) (px 1) (px 1) (px 1)
  ".task" ? do
    color (rgb 0 0 33)
    background white
    ".task" ? do
      noMargin
      ".parentPath" ? display none
    hover & do
      ".children" <? background lighterBlue
      ".uppertask" <? ".statusWrapper" ? background lighterBlue
  ".edit" ? visibility hidden
  ".righttask" ? hover & do
    width (pct 100)
    background veryLightBlue
  ".statusWrapper" ? do
    background lightBlue
    width leftBarWidth
    minWidth leftBarWidth
  ".uppertask" ? do
    display flex
    hover & ".edit" ? visibility visible
    hover & ".tag" ? ".edit" ? visibility hidden
    ".tag" ? hover & ".edit" ? visibility visible
  ".button" ? do
    hover & color blue
  i ? cursor cursorDefault
  ".icon" ? padding radius radius radius radius
  ".children" ? do
    background lightBlue
    padding (px 0) (px 0) (px 0) leftBarWidth
  ".slimButton" ? do
    marginRight (px (-5))
    marginLeft (px (-5))
  let blockSize = do
        width (em 1)
        height (em 1)
      bg = grayish 255
  ".checkbox" ? do
    marginTop (em 0.28)
    marginBottom (em 0.28)
    marginLeft (em 0.25)
    display inlineBlock
    fontSize (em 1.2)
    blockSize
    borderRadius (em 0.2) (em 0.2) (em 0.2) (em 0.2)
    background bg
    i ? do
      ".hide" & color bg
      ".grey" & color (grayish 160)
      ".show" & color black
      ".showable" & display none
    hover & i ? do
      ".hideable" & display none
      ".showable" & display inlineBlock
