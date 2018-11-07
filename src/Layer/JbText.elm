module Layer.JbText exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Product exposing (..)


shiftX = 320 -- half of the text width
shiftY = 40 -- half of the text height
defaultSize = 110
defaultWidth = 1500.0

view : Product -> (Int, Int) -> (Int, Int) -> Blend.Blend -> Html a
view product ( w, h ) ( x, y ) blend =
    let
        scale = toFloat w / defaultWidth
        posX = (toFloat w / 2) - toFloat x - shiftX
        posY = (toFloat h / 2) - toFloat y - shiftY
    in
        div
            [ HAttrs.class "text-layer"
            , HAttrs.style
                [ ("mix-blend-mode", Blend.encode blend)
                , ("position", "absolute")
                , ("top", toString posY ++ "px")
                , ("left", toString posX ++ "px")
                , ("transform", "scale(" ++ toString scale ++ ")")
                , ("font-size", toString defaultSize ++ "px")
                ]
            ]
            [ div
                [ HAttrs.class "text-layer text-layer--jetbrains"
                ,  HAttrs.style [ ( "max-width", "800px" ) ]
                , HAttrs.contenteditable True
                ]
            [ text <| getName product ]
            , span [ HAttrs.class "text-layer text-layer--subtitle" ] [ text  "Subtitle" ]
            ]
