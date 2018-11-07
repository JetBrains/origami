module ImportExport exposing
    ( encodeModel
    , decodeModel
    , EncodedState
    )

import Json.Decode as D exposing (bool, int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (decode, required, optional, hardcoded)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import WebGL.Blend as WGLBlend
-- import Svg.Blend as SVGBlend

import Model as M

type alias EncodedState = String


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


encodeLayerDef : M.LayerDef -> E.Value
encodeLayerDef layer =
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

encodeLayer : M.Layer -> E.Value
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


encodeModel_ : M.Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "theta", E.float model.theta )
        , ( "layers", E.list (List.map encodeLayerDef model.layers) )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        , ( "size", encodeIntPair model.size )
        , ( "origin", encodeIntPair model.size )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "now", E.float model.now )
        ]


encodeModel : M.Model -> EncodedState
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


intPairDecoder : D.Decoder (Int, Int)
intPairDecoder =
    D.decode (\i1 i2 -> (i1, i2))
        |> D.required "v1" D.int
        |> D.required "v2" D.int


layerDefsDecoder : D.Decoder (List M.LayerDef)
layerDefsDecoder =
    let
        createLayerDef kind blend isOn =
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
            ( D.decode createLayerDef
                |> D.required "kind" D.string
                |> D.required "blend" D.string
                |> D.required "isOn" D.bool
            )


layerDefDecoder : D.Decoder M.LayerDef
layerDefDecoder =
    D.decode M.LayerDef
        |> D.required "theta" D.float
        |> D.required "layers" layerDefsDecoder
        |> D.required "size" intPairDecoder
        |> D.required "origin" intPairDecoder
        |> D.required "mouse" intPairDecoder
        |> D.required "now" D.float


layerDecoder : D.Decoder M.Layer
layerDecoder =
    let
        createLayer = {}
    in
        D.decode createLayer
            |> D.required "theta" D.float
            |> D.required "layers" layerDefsDecoder
            |> D.required "size" intPairDecoder
            |> D.required "origin" intPairDecoder
            |> D.required "mouse" intPairDecoder
            |> D.required "now" D.float


modelDecoder : M.CreateLayer -> D.Decoder M.Model
modelDecoder createLayer =
    let
        createModel theta layers size origin mouse now =
            let
                initialModel = M.init [] createLayer
            in
                { initialModel
                | theta = theta
                , layers = layers
                , size = size
                , origin = origin
                , mouse = mouse
                , now = now
                }
    in
        D.decode createModel
            |> D.required "theta" D.float
            |> D.required "layers" layerDefsDecoder
            |> D.required "size" intPairDecoder
            |> D.required "origin" intPairDecoder
            |> D.required "mouse" intPairDecoder
            |> D.required "now" D.float


decodeModel : M.CreateLayer -> EncodedState -> Maybe M.Model
decodeModel createLayer modelStr =
    D.decodeString (modelDecoder createLayer) modelStr
        |> Debug.log "Decode Result: "
        |> Result.toMaybe
