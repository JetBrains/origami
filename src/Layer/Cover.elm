module Layer.Cover exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Product exposing (..)
import Json.Encode as E

import Model exposing (UiMode(..))
import Product 


shiftX = 320 -- half of the text width
shiftY = 40 -- half of the text height
defaultSize = 110
defaultWidth = 1500.0
imageWidth : Int
imageWidth = 120
imageHeight : Int
imageHeight = 120
scaleFactor : Float
scaleFactor = 0.1


view : UiMode -> Product -> (Int, Int) -> (Int, Int) -> Blend.Blend -> Html a
view mode product ( w, h ) ( x, y ) blend =
    let
        scale = toFloat w / defaultWidth
        posX = (toFloat w / 2) - toFloat x - shiftX
        posY = (toFloat h / 2) - toFloat y - shiftY
        logoPath = case Product.getLogoPath Product.JetBrains of
            Just fileName -> "./assets/" ++ fileName
            Nothing -> ""   
        textPath = case Product.getTextLinePath product of
            Just fileName -> "./assets/" ++ fileName
            Nothing -> ""                    
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
                , ("font-family", "'Gotham', Helvetica, sans-serif")
                , ("font-weight", "170")
                -- , ("text-transform", "uppercase")
                , ("color", "white")
                ]
            ]
            ( if mode == Production then 
                [ productName product posX posY textPath blend scale
                , logo product posX posY logoPath blend scale 
                ] 
              else 
                [ title product
                , logo product posX posY logoPath blend scale 
                ] 
            )


productName : Product -> Float -> Float -> String -> Blend.Blend -> Float -> Html a
productName product posX posY logoPath blend scale =
    let
        ( imageWidth, imageHeight ) = case product of 
            IntelliJ -> ( 300, 50 )
            _ -> ( 120, 120 )
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
                , ("top", "0")
                , ("left", "0")            
                -- , ("top", toString posY ++ "px")
                -- , ("left", toString posX ++ "px")
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


title : Product -> Html a 
title product = 
    div
        [ HAttrs.class
            ("text-layer--title text-layer--" ++ Product.encode product)
        ,  HAttrs.style
            [ ("max-width", "800px") ]
        , HAttrs.contenteditable True
        ]
        [ text <| getName product ]


logo : Product -> Float -> Float -> String -> Blend.Blend -> Float -> Html a
logo product posX posY logoPath blend scale =
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