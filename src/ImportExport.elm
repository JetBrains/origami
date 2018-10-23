module ImportExport exposing
    ( encodeModel
    , decodeModel
    , Model
    , Layer, LayerType(..)
    , EncodedState
    , PortBlend
    , defaultLayer
    )

import Time exposing (Time)

import Json.Decode as D exposing (int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (decode, required, optional, hardcoded)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

type alias EncodedState = String


-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> SVG Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend =
    ( Maybe WGLBlend.Blend, Maybe SVGBlend.PortBlend )


type LayerType
    = Unknown
    | Fss


-- type Config
--     = None
--     | FssConfig
--         FSS.Config
--     | MirroredFssConfig FSS.MConfig


type alias Layer =
    { type_ : LayerType
    , blend : WGLBlend.Blend
    --, config : Config
    --, mesh : FSS.Mesh
    }


type alias Model =
    { theta : Float
    , layers : List Layer
    , size : (Int, Int)
    , origin : (Int, Int)
    , mouse : (Int, Int)
    , now : Time
    }


encodeIntPair : ( Int, Int ) -> E.Value
encodeIntPair ( v1, v2 ) =
    E.object
        [ ( "v1", E.int v1 )
        , ( "v2", E.int v2 )
        ]


encodeLayerType : LayerType -> E.Value
encodeLayerType type_ =
    E.string
        (case type_ of
            Unknown -> "unknown"
            Fss -> "fss")


encodeLayer : Layer -> E.Value
encodeLayer layer =
    E.object
        [ ( "type", encodeLayerType layer.type_ )
        , ( "blend", WGLBlend.encodeOne layer.blend |> E.string )
        , ( "blendDesc",
            layer.blend
                |> WGLBlend.encodeHumanOne { delim = "; ", space = "> " }
                |> E.string
          )
        , ( "config", E.string "" )
        -- , ( "mesh", E.string "" )
        ]


encodeModel_ : Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "theta", E.float model.theta )
        , ( "layers", E.list (List.map encodeLayer model.layers) )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        , ( "size", encodeIntPair model.size )
        , ( "origin", encodeIntPair model.size )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "now", E.float model.now )
        ]


encodeModel : Model -> EncodedState
encodeModel model = model |> encodeModel_ |> E.encode 2


determineLayerType : String -> LayerType
determineLayerType layerTypeStr =
    case layerTypeStr of
        "fss" -> Fss
        _ -> Unknown


defaultLayer : Layer
defaultLayer =
    { type_ = Unknown
    , blend = WGLBlend.default
    -- , config = None
    --, mesh = FSS.emptyMesh
    }


layersDecoder : D.Decoder (List Layer)
layersDecoder =
    let
        createLayer type_ blend config =
            { type_ = determineLayerType type_
            , blend = WGLBlend.decodeOne blend
                |> Debug.log "Blend: "
                |> Maybe.withDefault WGLBlend.default
            --, config = FSS.init
            -- , mesh = FSS.emptyMesh
            }
    in
        D.list
            ( D.decode createLayer
                |> D.required "type_" D.string
                |> D.required "blend" D.string
                |> D.required "config" D.string
                --|> D.required "mesh" D.string
            )


intPairDecoder : D.Decoder (Int, Int)
intPairDecoder =
    D.decode (\i1 i2 -> (i1, i2))
        |> D.required "v1" D.int
        |> D.required "v2" D.int


modelDecoder : D.Decoder Model
modelDecoder =
    D.decode Model
        |> D.required "theta" D.float
        |> D.required "layers" layersDecoder
        |> D.required "size" intPairDecoder
        |> D.required "origin" intPairDecoder
        |> D.required "mouse" intPairDecoder
        |> D.required "now" D.float


decodeModel : EncodedState -> (Model -> a) -> Maybe a
decodeModel modelStr f =
    D.decodeString modelDecoder modelStr
        |> Debug.log "Decode Result: "
        |> Result.toMaybe
        |> Maybe.map f
