port module LayersNode exposing (..)

import Browser
import Dict as Dict exposing (Dict)
import Array as Array exposing (Array)
import Html
import Html.Attributes as HA exposing (attribute)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (..)

import WebGL.Blend as WGLB
import Html.Blend as HTMLB

type Blend
    = None
    | WebGLBlend WGLB.Blend
    | HtmlBlend HTMLB.Blend


-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> SVG Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend = (Maybe WGLB.Blend, Maybe HTMLB.PortBlend)

type alias Blends = Dict.Dict Int Blend

type alias Colors = Dict Int (Array String)


type alias Model =
    { layerCount : Int
    , size : ( Int, Int )
    , blends : Blends
    , colors : Colors
    }


type Msg
    = ChangeBlend Int Blend
    | ApplyAllBlends String
    | ApplyColors Colors
    | ChangeLayerCount Int
    | SetBlendType Int String
    | Resize ( Int, Int )


convertBlend : Blend -> PortBlend
convertBlend blend =
    case blend of
        None -> ( Nothing, Nothing )
        WebGLBlend webglBlend -> ( Just webglBlend, Nothing )
        HtmlBlend htmlBlend -> ( Nothing, Just (HTMLB.encode htmlBlend) )


adaptBlend : PortBlend -> Blend
adaptBlend portBlend =
    case portBlend of
        ( Just webGlBlend, Nothing ) -> WebGLBlend webGlBlend
        ( Nothing, Just htmlBlend ) -> HtmlBlend (HTMLB.decode htmlBlend )
        _ -> None


decodeOne : String -> Blend
decodeOne str =
    if (String.startsWith "_" str) then
        HTMLB.decode (String.dropLeft 1 str) |> HtmlBlend
    else
        case str of
            "" -> None
            someStr ->
                (WGLB.decodeOne someStr)
                    |> Maybe.map WebGLBlend
                    |> Maybe.withDefault None


encodeOne : Blend -> String
encodeOne blend =
    case blend of
        None -> "-"
        WebGLBlend webglBlend -> WGLB.encodeOne webglBlend
        HtmlBlend htmlBlend -> "_" ++ HTMLB.encode htmlBlend


decodeAll : String -> List Blend
decodeAll src =
    src
        |> String.split ":"
        |> List.map decodeOne


encodeAll : List Blend -> String
encodeAll blends =
    blends |> List.map encodeOne |> String.join ":"


move : Int -> Int -> Svg.Attribute Msg
move x y =
    transform ("translate(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")")


renderBlendFrom : Blends -> Int -> Int -> Svg Msg
renderBlendFrom blends count idx =
    g
        [ SA.style "alignment-baseline: hanging;"
        , class ("layer layer-" ++ String.fromInt idx)
        , move 0 ((count - idx - 1) * 90)
        ]
        [ text_ [ fill "black" ] [ text ("Layer " ++ String.fromInt idx) ]
        , blends
            |> Dict.get idx
            |> Maybe.withDefault None
            |> renderBlend idx
        ]


renderBlend : Int -> Blend -> Svg Msg
renderBlend idx blend =
    case blend of
        WebGLBlend webglBlend ->
            g
                [ class "blend", move 0 10 ]
                [ rect [ width "10", height "10", fill (getFill webglBlend)
                    , stroke "black", strokeWidth "1", rx "3", ry "3", move 1 4 ] []
                -- , text_ [ fill "black", move 15 5 ] [ text "Color EQ" ]
                , g
                    [ class "color-eq", move 22 8 ]
                    [ webglBlend.colorEq |> renderEq "color"
                    (\eq -> ChangeBlend idx (WebGLBlend { webglBlend | colorEq = eq })) ]
                --, text_ [ fill "black", move 15 55 ] [ text "Alpha EQ" ]
                , g
                    [ class "alpha-eq", move 22 45 ]
                    [ webglBlend.alphaEq |> renderEq "alpha"
                    (\eq -> ChangeBlend idx (WebGLBlend { webglBlend | alphaEq = eq })) ]
                ]
        HtmlBlend htmlBlend ->
            g
                [ class "blend", move 0 10 ]
                [ text_ [] [ text ("SVG:" ++ HTMLB.encode htmlBlend) ] ]
        None ->
            g
                [ class "blend", move 0 10 ]
                [ text_ [] [ text "None" ] ]


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
        |> (\c -> "rgba(" ++ String.fromFloat c.r ++ "," ++ String.fromFloat c.g ++ ","
                          ++ String.fromFloat c.b ++ "," ++ String.fromFloat c.a ++ ")")


init : ( Model, Cmd Msg )
init =
    (
        { layerCount =  0
        , size = ( 100, 100 )
        , blends = Dict.empty
        , colors = Dict.empty
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBlend layerId newBlend ->
            let
                model_
                    = { model
                    | blends = model.blends |> Dict.insert layerId newBlend
                    }
            in
                ( model_
                , Cmd.batch
                    [ sendNewBlend { layer = layerId, blend = convertBlend newBlend }
                    , sendNewCodeFrom model_.blends
                    ]
                )
        ApplyAllBlends blends ->
            let newBlends =
                    decodeAll blends
                        -- TODO: do not convert twice
                        |> Array.fromList
                        |> Array.toIndexedList
                model_ =
                    { model
                    | blends = Dict.fromList newBlends
                    }
                sendOneBlend (layerId, newBlend) =
                    sendNewBlend { layer = layerId, blend = convertBlend newBlend }
            in
                ( model_
                , Cmd.batch
                    ((newBlends |> List.map sendOneBlend) ++ [ sendNewCodeFrom model_.blends ])
                )
        ApplyColors colors ->
            ( { model
              | colors = colors
              }
            , Cmd.batch <|
                let
                    colors_ =
                        Dict.map
                            (\layerId layerColors ->
                                sendNewColors { layer = layerId, colors = layerColors }
                            ) colors
                in colors_ |> Dict.values
            )
            -- ! ( colors
            --         |> Dict.map
            --             (\layerId colors ->
            --                 sendNewColors { layer = layerId, colors = colors }
            --             )
            --         |> Dict.values
            --   )
        ChangeLayerCount newCount ->
            (
                { model
                | layerCount = newCount
                , blends =
                    List.range 0 (model.layerCount - 1)
                        |> List.map (\idx ->
                            ( idx
                            , model.blends
                                    |> Dict.get idx
                                    |> Maybe.withDefault None
                            )
                        )
                        |> Dict.fromList
                }
            , Cmd.none
            )
        SetBlendType layerId blendType ->
            let
                curBlend = Dict.get layerId model.blends |> Maybe.withDefault None
                newBlend = case blendType of
                    "webgl" -> case curBlend of
                                   WebGLBlend _ -> curBlend
                                   _ -> WebGLBlend WGLB.default
                    "html" -> case curBlend of
                                   HtmlBlend _ -> curBlend
                                   _ -> HtmlBlend HTMLB.default
                    _ -> None
                model_ =
                    { model
                    | blends = model.blends |> Dict.insert layerId newBlend
                    }
            in
                ( model_, Cmd.batch
                    [ sendNewBlend { layer = layerId, blend = convertBlend newBlend }
                    , sendNewCodeFrom model_.blends
                    ]
                )
        Resize newSize -> ( { model | size = newSize }, Cmd.none )


queueBlends : List (Int, Blend) -> List (Cmd Msg)
queueBlends blends =
    blends |> List.map
        (\(layerId, newBlend) ->
            sendNewBlend { layer = layerId, blend = convertBlend newBlend }
        )

sendNewCodeFrom : Blends -> Cmd Msg
sendNewCodeFrom blends = Dict.values blends |> encodeAll |> sendNewCode


adaptColors : Array (Array String) -> Colors
adaptColors source =
    source
        |> Array.toIndexedList
        |> Dict.fromList


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ changeBlend (\{ layer, blend } ->
            ChangeBlend layer (blend |> adaptBlend)
        )
        , setBlendType (\{ layer, blendType } ->
            SetBlendType layer blendType
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
        (case size of ( w, h ) -> [ width (String.fromInt w), height (String.fromInt h) ])
        (List.range 0 (layerCount - 1) |> List.map (renderBlendFrom blends layerCount) )


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port changeLayerCount : (Int -> msg) -> Sub msg

port setBlendType :
    ( { layer : Int
      , blendType : String
      }
    -> msg) -> Sub msg

port applyColors : (Array (Array String) -> msg) -> Sub msg

port resize : ( ( Int, Int ) -> msg ) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : PortBlend
      }
    -> msg) -> Sub msg

port applyAllBlends :
    ( String
    -> msg) -> Sub msg

port sendNewBlend :
    { layer: Int
    , blend: PortBlend
    } -> Cmd msg

port sendNewColors :
    { layer: Int
    , colors: Array String
    } -> Cmd msg

port sendNewCode :
    String -> Cmd msg
