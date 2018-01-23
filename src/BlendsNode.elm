port module BlendsNode exposing (..)

import Dict
import Array
import Html
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)

import Blend as B


type alias Blends = Dict.Dict Int B.Blend


type alias Model =
    { layerCount : Int
    , size: ( Int, Int )
    , blends: Blends
    }


type Msg
    = ChangeBlend Int B.Blend
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
        , move 0 (idx * 130)
        ]
        [ text_ [ fill "black" ] [ text ("Layer " ++ toString idx) ]
        , blends
            |> Dict.get idx
            |> Maybe.withDefault B.default
            |> renderBlend idx
        ]


renderBlend : Int -> B.Blend -> Svg Msg
renderBlend idx blend =
    g
        [ class "blend", move 0 10 ]
        [ rect [ width "10", height "10", fill (getFill blend)
               , stroke "black", strokeWidth "1", rx "3", ry "3", move 1 4 ] []
        , text_ [ fill "black", move 15 5 ] [ text "Color EQ" ]
        , g
            [ class "color-eq", move 15 15 ]
            [ blend.colorEq |> renderEq "color"
              (\eq -> ChangeBlend idx { blend | colorEq = eq }) ]
        , text_ [ fill "black", move 15 55 ] [ text "Alpha EQ" ]
        , g
            [ class "alpha-eq", move 15 65 ]
            [ blend.alphaEq |> renderEq "alpha"
              (\eq -> ChangeBlend idx { blend | alphaEq = eq }) ]
        ]


renderEq : String -> (B.Equation -> Msg) -> B.Equation -> Svg Msg
renderEq eqType upd ( func, factor1, factor2 ) =
    let
        updFunc = (\newFunc -> upd ( newFunc, factor1, factor2 ))
        updFact1 = (\newFact1 -> upd ( func, newFact1, factor2 ))
        updFact2 = (\newFact2 -> upd ( func, factor1, newFact2 ))
    in
        g
            [ class ("equation equation-" ++ eqType) ]
            [ g [ class "func" ]
                ( B.allFuncs |> Array.indexedMap (renderFunc updFunc func) |> Array.toList )
            , g [ class "factor-1", move 0 12 ]
                ( B.allFactors |> Array.indexedMap (renderFactor updFact1 factor1) |> Array.toList )
            , g [ class "factor-2", move 0 24 ]
                ( B.allFactors |> Array.indexedMap (renderFactor updFact2 factor2) |> Array.toList )
            ]


renderFunc : (Int -> Msg) -> Int -> Int -> a -> Svg Msg
renderFunc select curN n _ =
    text_
        [ SA.style "cursor: pointer;"
        , fill (if (n == curN) then "blue" else "white")
        , move (n * 30) 0
        , SE.onClick (select n)
        ]
        [ B.labelOfFunc n |> text
        ]


renderFactor : (Int -> Msg) -> Int -> Int -> a -> Svg Msg
renderFactor select curN n _ =
    text_
        [ SA.style "cursor: pointer;"
        , fill (if (n == curN) then "blue" else "white")
        , move (n * 30) 0
        , SE.onClick (select n)
        ]
        [ B.labelOfFactor n |> text
        ]


getFill : B.Blend -> String
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
    } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBlend layerId newBlend ->
            { model | blends = model.blends |> Dict.insert layerId newBlend }
            ! [ sendNewBlend { layer = layerId, blend = newBlend }]
        ChangeLayerCount newCount ->
            { model
            | layerCount = newCount
            , blends =
                List.range 0 (model.layerCount - 1)
                    |> List.map (\idx ->
                           ( idx, model.blends |> Dict.get idx |> Maybe.withDefault B.default )
                       )
                    |> Dict.fromList
            } ! []
        Resize newSize -> { model | size = newSize } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
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

port resize : ( ( Int, Int ) -> msg ) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : B.Blend
      }
    -> msg) -> Sub msg

port sendNewBlend :
    { layer: Int
    , blend: B.Blend
    } -> Cmd msg
