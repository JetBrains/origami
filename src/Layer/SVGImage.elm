module Layer.SVGImage exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Svg.Attributes as SAttrs
-- import InlineSvg exposing (inline)

import Product exposing (Product)

-- defaultSize = 110
imageWidth = 120
imageHeight = 120


view : (Int, Int) -> (Int, Int) -> Product -> Blend.Blend -> Html a
view ( w, h ) ( x, y ) product blend =
    let
        --scale = toFloat w / defaultWidth
        posX = (toFloat w) - toFloat x - (toFloat imageWidth) - 50
        posY = (toFloat h) - toFloat y - (toFloat imageHeight) - 50
        logoPath = case Product.getLogoPath product of
            Just fileName -> "./assets/" ++ fileName
            Nothing -> ""
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
                , ("background-image", "url(\"" ++ logoPath ++ "\")")
                , ("background-repeat", "no-repeat")
                , ("background-position", "center center")
                , ("background-size", "contain")
                ]
            ]
            [ span
                [ HAttrs.class "text-layer text-layer--jetbrains" ]
                [ {- text <| Product.getName product -} ]
            ]
