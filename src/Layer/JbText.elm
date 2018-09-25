module Layer.JbText exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Svg.Attributes as SAttrs
import InlineSvg exposing (inline)


--shiftX = 0
--shiftY = 0

svgWidth = 80
shiftX = 1300
shiftY = 500


view : (Int, Int) -> Blend.Blend -> Html a
view ( w, h ) blend =
    let
        posX = w - shiftX
        posY = h - shiftY
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
