module ImportExport exposing
    ( encodeModel
    , decodeModel
    , Model
    , Layer
    , EncodedState
    , defaultLayer
    )

import Time exposing (Time)

import Json.Decode as D exposing (bool, int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (decode, required, optional, hardcoded)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Model as M

type alias EncodedState = String

-- type Config
--     = None
--     | FssConfig
--         FSS.Config
--     | MirroredFssConfig FSS.MConfig


type alias Layer =
    { kind : M.LayerKind
    , blend : WGLBlend.Blend
    , isOn : Bool
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


encodeLayerType : M.LayerKind -> E.Value
encodeLayerType kind =
    E.string
        (case kind of
            M.Fss -> "fss"
            M.MirroredFss -> "fss-mirror"
            M.Lorenz -> "lorenz"
            M.Fractal -> "fractal"
            M.Template -> "template"
            M.Voronoi -> "voronoi"
            M.Text -> "text"
            M.SvgImage -> "logo"
            M.Vignette -> "vignette")


encodeLayer : Layer -> E.Value
encodeLayer layer =
    E.object
        [ ( "kind", encodeLayerType layer.kind )
        , ( "blend", WGLBlend.encodeOne layer.blend |> E.string )
        , ( "blendDesc",
            layer.blend
                |> WGLBlend.encodeHumanOne { delim = "; ", space = "> " }
                |> E.string
          )
        , ( "isOn", layer.isOn |> E.bool )
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


determineLayerType : String -> M.LayerKind
determineLayerType layerTypeStr =
    case layerTypeStr of
        "fss" -> M.Fss
        "fss-mirror" -> M.MirroredFss
        "lorenz" -> M.Lorenz
        "fractal" -> M.Fractal
        "template" -> M.Template
        "voronoi" -> M.Voronoi
        "text" -> M.Text
        "logo" -> M.SvgImage
        "vignette" -> M.Vignette
        _ -> M.Template


defaultLayer : Layer
defaultLayer =
    { kind = M.Template
    , blend = WGLBlend.default
    , isOn = True
    -- , config = None
    --, mesh = FSS.emptyMesh
    }


layersDecoder : D.Decoder (List Layer)
layersDecoder =
    let
        createLayer kind blend isOn =
            { kind = determineLayerType kind
            , blend = WGLBlend.decodeOne blend
                |> Debug.log "Blend: "
                |> Maybe.withDefault WGLBlend.default
            , isOn = isOn
            --, config = FSS.init
            -- , mesh = FSS.emptyMesh
            }
    in
        D.list
            ( D.decode createLayer
                |> D.required "kind" D.string
                |> D.required "blend" D.string
                |> D.required "isOn" D.bool
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
