module Layer.Cover exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes exposing (style, class, attribute, contenteditable)
import Html.Blend as Blend
import Product exposing (..)
import Json.Encode as E

import Model exposing (UiMode(..))
import Product


defaultSize = 110
defaultWidth = 1500.0
-- imageWidth : Int
-- imageWidth = 120
-- imageHeight : Int
-- imageHeight = 120
scaleFactor : Float
scaleFactor = 0.1


view : UiMode -> Product -> (Int, Int) -> (Int, Int) -> Blend.Blend -> Html a
view mode product ( w, h ) ( x, y ) blend =
    let
        scale = toFloat w / defaultWidth
        centerX = (toFloat w / 2) - toFloat x
        centerY = (toFloat h / 2) - toFloat y
        logoX = toFloat w - toFloat x - 0.1 * toFloat h
        logoY = toFloat h - toFloat y - 0.1 * toFloat h
    in
        div
            [ class "cover-layer"
            , style "mix-blend-mode" <| Blend.encode blend
            , style "position" "absolute"
            , style "top" "0px"
            , style "left" "0px"
            , style "font-size" <| String.fromInt defaultSize ++ "px"
            , style "font-family" "'Gotham', Helvetica, sans-serif"
            , style "font-weight" "170"
                -- , ("text-transform", "uppercase")
            , style "color" "white"
            ]
        ( if (mode == Production) || (mode == TronUi Production) then
            [ productName product ( centerX, centerY ) blend scale
            , logo ( logoX, logoY ) blend scale
            ]
          else
            [
            -- title product
            --, logo product posX posY logoPath blend scale
            ]
        )



productName : Product -> ( Float, Float ) -> Blend.Blend -> Float -> Html a
productName product pos blend scale =
    let
        textPath =
            case Product.getTextLinePath product of
                Just fileName -> "./assets/" ++ fileName
                Nothing -> ""
        textSize = Product.getCoverTextSize product
    in
        image
            textPath
            ("product-name-layer product-name-layer-" ++ Product.encode product)
            pos
            textSize
            blend
            scale



logo : ( Float, Float ) -> Blend.Blend -> Float -> Html a
logo ( logoX, logoY ) blend scale =
    let
        logoPath =
            case Product.getLogoPath Product.JetBrains of
                Just fileName -> "./assets/" ++ fileName
                Nothing -> ""
        ( logoWidth, logoHeight ) = ( 90, 90 )
    in image
            logoPath
            ("logo-layer logo-layer-" ++ Product.encode JetBrains)
            ( logoX, logoY )
            ( logoWidth, logoHeight )
            blend
            scale


image : String -> String -> ( Float, Float ) -> ( Int, Int ) -> Blend.Blend -> Float -> Html a
image imagePath imgClass ( posX, posY ) ( imageWidth, imageHeight ) blend scale =
    div
        [ class imgClass
        ,
            { blend = Blend.encode blend
            , posX = posX
            , posY = posY
            , width = imageWidth
            , height = imageHeight
            , imagePath = imagePath
            , scale = scale
            }
            |> encodeStoredData
            |> E.encode 0
            |> attribute "data-stored"
        , style "mix-blend-mode" <| Blend.encode blend
        , style "position" "absolute"
        , style "top" "0px"
        , style "left" "0px"
        , style "width" <| String.fromFloat ( toFloat imageWidth * scale ) ++ "px"
        , style "height" <| String.fromFloat ( toFloat imageHeight * scale ) ++ "px"
        , style "transform" <| "translate("
                ++ String.fromFloat (posX - (toFloat imageWidth * scale) / 2.0) ++ "px, "
                ++ String.fromFloat (posY - (toFloat imageHeight * scale) / 2.0) ++ "px)"
        , style "background-image" <| "url(\"" ++ imagePath ++ "\")"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center center"
        , style "background-size" "contain"
        ]
        [ --img [ HAttrs.src logoPath, HAttrs.attribute "crossorigin" "anonymous" ] []
        ]


title : Product -> Html a
title product =
    div
        [ class
            ("text-layer--title text-layer--" ++ Product.encode product)
        , style "max-width" "800px"
--            ,("mix-blend-mode", Blend.encode blend)
--                , ("position", "absolute")
--                , ("top", toString posY ++ "px")
--                , ("left", toString posX ++ "px")
--                , ("transform", "scale(" ++ toString scale ++ ")")
        , style "font-size" <| String.fromInt defaultSize ++ "px"
        , style "font-family" "'Gotham', Helvetica, sans-serif"
        , style "font-weight" "170"
              -- , ("text-transform", "uppercase")
        , style "color" "white"
        , contenteditable True
        ]
        [ text <| getName product ]


type alias StoredData =
    { scale : Float
    , posX : Float
    , posY : Float
    , blend : String
    , imagePath : String
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
        , ( "imagePath", E.string s.imagePath )
        , ( "width", E.int s.width )
        , ( "height", E.int s.height )
        ]
