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
scaleFactor = 0.1

view : (Int, Int) -> (Int, Int) -> Product -> Blend.Blend -> Html a
view ( w, h ) ( x, y ) product blend =
    let
        scale = toFloat w * scaleFactor / imageWidth
        posX = (toFloat w) - toFloat x - (toFloat imageWidth * scale) - (30 * scale)
        posY = (toFloat h) - toFloat y - (toFloat imageHeight * scale) - (30 * scale)
        logoPath = case Product.getLogoPath Product.JetBrains of
            Just fileName -> "./assets/" ++ fileName
            Nothing -> ""
    in
        div
            [ HAttrs.class ("logo-layer logo-layer--" ++ Product.encode product)
            , HAttrs.style
                [ ("mix-blend-mode", Blend.encode blend)
                , ("position", "absolute")
                -- , ("top", toString posY ++ "px")
                -- , ("left", toString posX ++ "px")
                , ("top", "0")
                , ("left", "0")
                , ("width", toString imageWidth ++ "px")
                , ("height", toString imageHeight ++ "px")
                --, ("transform", "scale(" ++ toString scale ++ ")")
                -- , ("font-size", toString defaultSize ++ "px")
                , ("background-image", "url(\"" ++ logoPath ++ "\")")
                , ("background-repeat", "no-repeat")
                , ("background-position", "center center")
                , ("background-size", "contain")
                , ("transform", "scale(" ++ toString scale ++ ")")
                ]
            ]
            [
            ]
