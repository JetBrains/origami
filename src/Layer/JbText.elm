module Layer.JbText exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Svg.Attributes as SAttrs
import InlineSvg exposing (inline)


svgWidth = 80
shiftX = 320 -- half of the text width
shiftY = 40 -- half of the text height


view : (Int, Int) -> (Int, Int) -> Blend.Blend -> Html a
view ( w, h ) ( x, y ) blend =
    let
        posX = (toFloat w / 2) - toFloat x - shiftX
        posY = (toFloat h / 2) - toFloat y - shiftY
    in
        div
            [ HAttrs.style
                [ ("mix-blend-mode", Blend.encode blend)
                , ("position", "absolute")
                , ("top", toString posY ++ "px")
                , ("left", toString posX ++ "px")
                ]
            ]
            [ span [ HAttrs.class "text-layer text-layer--jetbrains" ] [ text "JetBrains" ]
            ]
