{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Clay
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import Prelude hiding ((**))

font_handwritten :: Css
font_handwritten = fontFamily ["Segoe Print", "Bradley Hand", "Chilanka", "TSCu_Comic", "casual"] [cursive]

link, link_active :: Color
link = "#7FB4CA"
link_active = "#E82424"

tag :: Text -> Color -> Css
tag name col =
  (".tags" <> "h1") ? do
    let tagname = element (".tag-" <> name)
    tagname ? do
      important (color col)
      before
        & background col
      a
        <? important (color col)

-- a
--   # ":active"
--   <? important (color link_active)

stylesheet :: Css
stylesheet = do
  tag "haskell" "#8F4E8B"
  tag "effect-ts" "#007f00"
  tag "typescript" "#98BB6C"
  tag "fsharp" "#FF9E3B"
  tag "graphics" "#E82424"
  tag "unity" "#16161D"
  tag "reverse-engineering" "#F05138"
  tag "macos" "#1771E4"
  tag "interop" "#8BC789"
  tag "robotics" "#6CBB82"
  tag "homelab" "#D9903A"
  tag "algorithms" "#7E3445"
  tag "low-level" "#7B403B"
  tag "life" "#EFAF8B"

main :: IO ()
main = T.putStr $ renderWith compact [] stylesheet
