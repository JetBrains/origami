port module LayersNode exposing (..)

import Dict as Dict exposing (Dict)
import Array as Array exposing (Array)
import Html
import Html.Attributes as HA exposing (attribute)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)

import WebGL.Blend as WGLB


type alias Blends = Dict.Dict Int WGLB.Blend

type alias Colors = Dict Int (Array String)


type alias Model =
    { layerCount : Int
    , size : ( Int, Int )
    , blends : Blends
    , colors : Colors
    }


type Msg
    = ChangeWGLBlend Int WGLB.Blend
    | ApplyAllBlends String
    | ApplyColors Colors
    | ChangeLayerCount Int
    | Resize ( Int, Int )


move : Int -> Int -> Svg.Attribute Msg
move x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")


renderBlendFrom : Blends -> Int -> Svg Msg
renderBlendFrom blends idx =
    g
        [ SA.style "alignment-baseline: hanging;"
        , class ("layer layer-" ++ toString idx)
        , move 0 (idx * 90)
        ]
        [ text_ [ fill "black" ] [ text ("Layer " ++ toString idx) ]
        , blends
            |> Dict.get idx
            |> Maybe.withDefault WGLB.default
            |> renderBlend idx
        ]


renderBlend : Int -> WGLB.Blend -> Svg Msg
renderBlend idx blend =
    g
        [ class "blend", move 0 10 ]
        [ rect [ width "10", height "10", fill (getFill blend)
               , stroke "black", strokeWidth "1", rx "3", ry "3", move 1 4 ] []
       -- , text_ [ fill "black", move 15 5 ] [ text "Color EQ" ]
        , g
            [ class "color-eq", move 22 8 ]
            [ blend.colorEq |> renderEq "color"
              (\eq -> ChangeWGLBlend idx { blend | colorEq = eq }) ]
        --, text_ [ fill "black", move 15 55 ] [ text "Alpha EQ" ]
        , g
            [ class "alpha-eq", move 22 45 ]
            [ blend.alphaEq |> renderEq "alpha"
              (\eq -> ChangeWGLBlend idx { blend | alphaEq = eq }) ]
        ]


renderEq : String -> (WGLB.Equation -> Msg) -> WGLB.Equation -> Svg Msg
renderEq eqType upd ( func, factor1, factor2 ) =
    let
        updFunc = (\newFunc -> upd ( newFunc, factor1, factor2 ))
        updFact1 = (\newFact1 -> upd ( func, newFact1, factor2 ))
        updFact2 = (\newFact2 -> upd ( func, factor1, newFact2 ))
    in
        g
            [ class ("equation equation-" ++ eqType) ]
            [ g [ class "func" ]
                ( WGLB.allFuncs |> Array.indexedMap (renderFunc updFunc func) |> Array.toList )
            , g [ class "factor-1", move 0 12 ]
                ( WGLB.allFactors |> Array.indexedMap (renderFactor updFact1 factor1) |> Array.toList )
            , g [ class "factor-2", move 0 24 ]
                ( WGLB.allFactors |> Array.indexedMap (renderFactor updFact2 factor2) |> Array.toList )
            ]


renderFunc : (Int -> Msg) -> Int -> Int -> a -> Svg Msg
renderFunc select curN n _ =
    circle
        [ SA.style "cursor: pointer;"
        , SA.r "3"
        , fill (if (n == curN) then "white" else "black")
        , move (n * 12) 0
        , HA.attribute "data-label" <| WGLB.labelOfFunc n
        , SE.onClick (select n)
        ]
        [ ]


renderFactor : (Int -> Msg) -> Int -> Int -> a -> Svg Msg
renderFactor select curN n _ =
    circle
        [ SA.style "cursor: pointer;"
        , SA.r "3"
        , fill (if (n == curN) then "white" else "black")
        , move (n * 12) 0
        , HA.attribute "data-label" <| WGLB.labelOfFactor n
        , SE.onClick (select n)
        ]
        [ ]


getFill : WGLB.Blend -> String
getFill { color } =
    color
        |> Maybe.withDefault { r = 0, g = 0, b = 0, a = 0 }
        |> (\c -> "rgba(" ++ toString c.r ++ "," ++ toString c.g ++ ","
                          ++ toString c.b ++ "," ++ toString c.a ++ ")")


init : ( Model, Cmd Msg )
init =
    { layerCount =  0
    , size = ( 100, 100 )
    , blends = Dict.empty
    , colors = Dict.empty
    } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeWGLBlend layerId newBlend ->
            let model_ = { model | blends = model.blends |> Dict.insert layerId newBlend }
            in
                model_ !
                    [ sendNewWGLBlend { layer = layerId, blend = newBlend }
                    , Dict.values model_.blends |> WGLB.encodeAll |> sendNewCode
                    ]
        ApplyAllBlends blends ->
            let newBlends =
                WGLB.decodeAll blends
                    -- TODO: do not convert twice
                    |> Array.fromList
                    |> Array.toIndexedList
            in
                { model | blends = Dict.fromList newBlends
                }
                ! (newBlends |> List.map
                    (\(layerId, newBlend) ->
                        sendNewWGLBlend { layer = layerId, blend = newBlend }
                    ))
        ApplyColors colors ->
            { model | colors = colors
            }
            ! (let colors_ =
                Dict.map
                    (\layerId colors ->
                        sendNewColors { layer = layerId, colors = colors }
                    ) colors
              in colors_ |> Dict.values)
            -- ! ( colors
            --         |> Dict.map
            --             (\layerId colors ->
            --                 sendNewColors { layer = layerId, colors = colors }
            --             )
            --         |> Dict.values
            --   )
        ChangeLayerCount newCount ->
            { model
            | layerCount = newCount
            , blends =
                List.range 0 (model.layerCount - 1)
                    |> List.map (\idx ->
                           ( idx, model.blends |> Dict.get idx |> Maybe.withDefault WGLB.default )
                       )
                    |> Dict.fromList
            } ! []
        Resize newSize -> { model | size = newSize } ! []


adaptColors : Array (Array String) -> Colors
adaptColors source =
    source
        |> Array.toIndexedList
        |> Dict.fromList


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ changeWGLBlend (\{ layer, blend } ->
            ChangeWGLBlend layer blend
          )
        , applyAllBlends (\encodedBlends ->
            ApplyAllBlends encodedBlends
          )
        , applyColors (\colors ->
            colors |> adaptColors |> ApplyColors
          )
        , resize Resize
        , changeLayerCount ChangeLayerCount
        ]


view : Model -> Html.Html Msg
view { layerCount, size, blends } =
    svg
        (case size of ( w, h ) -> [ width (toString w), height (toString h) ])
        (List.range 0 (layerCount - 1) |> List.map (renderBlendFrom blends) )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port changeLayerCount : (Int -> msg) -> Sub msg

port applyColors : (Array (Array String) -> msg) -> Sub msg

port resize : ( ( Int, Int ) -> msg ) -> Sub msg

port changeWGLBlend :
    ( { layer : Int
      , blend : WGLB.Blend
      }
    -> msg) -> Sub msg

port applyAllBlends :
    ( String
    -> msg) -> Sub msg

port sendNewWGLBlend :
    { layer: Int
    , blend: WGLB.Blend
    } -> Cmd msg

port sendNewColors :
    { layer: Int
    , colors: Array String
    } -> Cmd msg

port sendNewCode :
    String -> Cmd msg
