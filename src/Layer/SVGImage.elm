module Layer.SVGImage exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
-- import Svg.Attributes as SAttrs
-- import InlineSvg exposing (inline)

import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import Product exposing (Product)

-- defaultSize = 110
imageWidth : Int
imageWidth = 120
imageHeight : Int
imageHeight = 120
scaleFactor : Float
scaleFactor = 0.1


view : (Int, Int) -> (Int, Int) -> Product -> Blend.Blend -> Html a
view ( w, h ) ( x, y ) product blend =
    let
        scale = toFloat w * scaleFactor / toFloat imageWidth
        posX = (toFloat w) - toFloat x - (toFloat imageWidth * scale) - (30 * scale)
        posY = (toFloat h) - toFloat y - (toFloat imageHeight * scale) - (30 * scale)
        logoPath = case Product.getLogoPath Product.JetBrains of
            Just fileName -> "./assets/" ++ fileName
            Nothing -> ""
    in
        div
            [ HAttrs.class ("logo-layer logo-layer--" ++ Product.encode product)
            , { blend = Blend.encode blend
              , posX = posX
              , posY = posY
              , width = imageWidth
              , height = imageHeight
              , logoPath = logoPath
              , scale = scale
              }
              |> encodeStoredData
              |> E.encode 0
              |> HAttrs.attribute "data-stored"
            , HAttrs.style
                [ ("mix-blend-mode", Blend.encode blend)
                , ("position", "absolute")
                -- , ("transform", "scale(" ++ toString scale ++ ")")
                , ("top", toString posY ++ "px")
                , ("left", toString posX ++ "px")
                , ("width",  (if (imageWidth < 48) then "48" else toString ( toFloat imageWidth * scale )) ++ "px")
                , ("height",  (if (imageHeight < 48) then "48" else toString ( toFloat imageHeight * scale )) ++ "px")
                --, ("transform", "scale(" ++ toString scale ++ ")")
                -- , ("font-size", toString defaultSize ++ "px")
                , ("background-image", "url(\"" ++ logoPath ++ "\")")
                , ("background-repeat", "no-repeat")
                , ("background-position", "center center")
                , ("background-size", "contain")
                ]
            ]
            [ --img [ HAttrs.src logoPath, HAttrs.attribute "crossorigin" "anonymous" ] []
            ]


type alias StoredData =
    { scale : Float
    , posX : Float
    , posY : Float
    , blend : String
    , logoPath : String
    , width : Int
    , height : Int
    }


encodeStoredData : StoredData -> E.Value
encodeStoredData s =
    E.object
        [ ( "scale", E.float s.scale )
        , ( "posX", E.float s.posX )
        , ( "posY", E.float s.posY )
        , ( "blend", E.string s.blend )
        , ( "logoPath", E.string s.logoPath )
        , ( "width", E.int s.width )
        , ( "height", E.int s.height )
        ]
