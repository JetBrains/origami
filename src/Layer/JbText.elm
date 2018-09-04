module Layer.JbText exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Svg.Attributes as SAttrs
import InlineSvg exposing (inline)


svgWidth = 500
shiftX = 0
shiftY = 0


{ icon } =
    inline
        { jetbrains = "./jetbrains-text-square.svg"
        }

view : (Int, Int) -> Blend.Blend -> Html a
view ( w, h ) blend =
    let
        posX = (w // 2) - shiftX
        posY = (h // 2) - shiftY
    in
        div
            [ HAttrs.style
                [ ("mix-blend-mode", Blend.encode blend)
                , ("position", "absolute")
                , ("width", toString svgWidth ++ "px")
                , ("top", toString posY ++ "px")
                , ("left", toString posX ++ "px")
                ] ]
            [ icon .jetbrains [ SAttrs.class "text-layer text-layer--jetbrains" ]
            ]
