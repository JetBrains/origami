module Layer.SVGImage exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Svg.Attributes as SAttrs
-- import InlineSvg exposing (inline)

-- defaultSize = 110
imageWidth = 200
imageHeight = 200

view : (Int, Int) -> (Int, Int) -> Blend.Blend -> Html a
view ( w, h ) ( x, y ) blend =
    let
        --scale = toFloat w / defaultWidth
        posX = (toFloat w / 2) - toFloat x - (toFloat imageWidth / 2)
        posY = (toFloat h / 2) - toFloat y - (toFloat imageHeight / 2)
    in
        div
            [ HAttrs.style
                [ ("mix-blend-mode", Blend.encode blend)
                , ("position", "absolute")
                , ("top", toString posY ++ "px")
                , ("left", toString posX ++ "px")
                , ("width", toString imageWidth ++ "px")
                , ("height", toString imageHeight ++ "px")
                --, ("transform", "scale(" ++ toString scale ++ ")")
                -- , ("font-size", toString defaultSize ++ "px")
                , ("background-image", "url(\"./assets/jetbrains.svg\")")
                , ("background-repeat", "no-repeat")
                , ("background-position", "center center")
                , ("background-size", "contain")
                ]
            ]
            [ span [ HAttrs.class "text-layer text-layer--jetbrains" ] [ ]
            ]
