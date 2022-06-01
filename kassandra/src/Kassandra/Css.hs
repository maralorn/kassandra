module Kassandra.Css (
  cssAsBS,
  cssAsText,
) where

import Clay
import Prelude hiding (
  i,
  (&),
  (|>),
 )

cssToBS :: Css -> ByteString
cssToBS = encodeUtf8 . cssToText

cssToText :: Css -> Text
cssToText = toStrict . render

cssAsBS :: ByteString
cssAsBS = $$([||cssToBS (css (Just ""))||])

cssAsText :: Text -> Text
cssAsText fontPath = cssToText (css (Just fontPath))

fontName :: Text
fontName = "Material Icons"

css :: Maybe Text -> Css
css fontPath = do
  whenJust fontPath $ \fontSrc -> do
    fontFace $ do
      fontFamily [fontName] []
      fontFaceSrc [FontFaceSrcUrl fontSrc (Just OpenType)]
  let --darkBlue      = rgb 0 0 33
      lightBlue = rgb 200 200 255
      noMargin = margin (px 0) (px 0) (px 0) (px 0)
      noPadding = padding (px 0) (px 0) (px 0) (px 0)
  star ? do
    fontFamily ["B612"] []
    noMargin
    noPadding
  body ? do
    background white
    color black
    minHeight (pct 100)
  ".definitionElement" ? do
    clear clearLeft
  ".definitionUI" ? do
    float floatLeft
  ".remoteBackend" ? do
    textAlign (alignSide sideRight)
  ".loginDialog" ? do
    textAlign (alignSide sideCenter)
    padding (em 5) (em 5) (em 5) (em 5)
  ".header" ? do
    padding (em 0.2) (em 0.2) (em 0.2) (em 0.2)
    background lightBlue
    fontWeight bold
    fontSize (em 1.5)
  ".content" ? do
    paddingBottom (em 4)
  ".footer" ? do
    position fixed
    bottom (px 0)
    width (pct 100)
    padding (em 0.22) (em 0.2) (em 0.2) (em 0.2)
    background (grayish 200)
    color black
  ".container" ? do
    display flex
    minHeight (pct 100)
  ".dropHere" ? do
    position absolute
    background white
    color black
    border (em 0.1) solid black
  let offset = 2
  ".plusOne" ? marginLeft (em offset)
  ".plusTwo" ? marginLeft (em (offset * 2))
  ".above" ? marginTop (em (-0.8))
  ".pane" ? width (pct 100)
  let buttonPadding =
        padding (em 0.3) (em 0.5) (em 0.3) (em 0.5)
      buttonCss = do
        display inlineBlock
        margin (px 1) (px 1) (px 1) (px 1)
        buttonPadding
        border (em 0.1) solid black
        active & do
          background black
          color white
  ".button" ? buttonCss
  ".selector" ? buttonCss
  --".tag" ? ".icon" ? do
  --position absolute
  --borderRadius tagRadius tagRadius tagRadius tagRadius
  --background lightBlue
  --marginLeft (em (-1.1))
  --marginTop (em 0.70)
  --fontSize (em 0.85)
  ".material-icons" ? do
    fontFamily [fontName] []
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
  let radius = em 0.3
      leftBarWidth = em 1.8
  ".activeEdit" ? buttonPadding
  ".event" ? do
    border (px 1) solid black
  ".task" ? do
    color (rgb 0 0 33)
    border (px 1) solid black
    background white
    ".task" ? do
      noMargin
      ".parentPath" ? display none
  ".righttask" ? do
    width (pct 100)
  ".statusWrapper" ? do
    background black
    width leftBarWidth
    minWidth leftBarWidth
  ".uppertask" ? do
    display flex
    ".edit" ? visibility visible
  i ? cursor cursorDefault
  ".icon" ? padding radius radius radius radius
  ".children" ? do
    padding (px 0) (px 0) (px 0) leftBarWidth
    background black
  --".slimButton" ? do
  --marginRight (px (-5))
  --marginLeft (px (-5))
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
    active & i ? do
      background black
