module ImportExport exposing
    ( encodeModel
    , decodeModel
    , EncodedState
    , encodePortModel
    , encodeFss
    , fromFssPortModel
    , encodeFssRenderMode
    , decodeFssRenderMode
    )

import Array

import Json.Decode as D exposing (bool, int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (decode, required, optional, hardcoded)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Layer.FSS as FSS
import Product exposing (..)

import Model as M

type alias EncodedState = String


encodeIntPair : ( Int, Int ) -> E.Value
encodeIntPair ( v1, v2 ) =
    E.object
        [ ( "v1", E.int v1 )
        , ( "v2", E.int v2 )
        ]


encodePairAsArray : (a -> E.Value) -> ( a, a ) -> E.Value
encodePairAsArray f ( v1, v2 ) =
   [ v1, v2 ]
        |> List.map f
        |> Array.fromList
        |> E.array


encodeTripleAsArray : (a -> E.Value) -> ( a, a, a ) -> E.Value
encodeTripleAsArray f ( v1, v2, v3 ) =
   [ v1, v2, v2 ]
        |> List.map f
        |> Array.fromList
        |> E.array


encodeKind_ : M.LayerKind -> String
encodeKind_ kind =
    case kind of
        M.Fss -> "fss"
        M.MirroredFss -> "fss-mirror"
        M.Lorenz -> "lorenz"
        M.Fractal -> "fractal"
        M.Template -> "template"
        M.Voronoi -> "voronoi"
        M.Text -> "text"
        M.SvgImage -> "logo"
        M.Vignette -> "vignette"
        M.Empty -> "empty"


encodeKind : M.LayerKind -> E.Value
encodeKind = E.string << encodeKind_


encodeLayerDef : M.LayerDef -> E.Value
encodeLayerDef layerDef =
    E.object
        [ ( "kind", encodeKind layerDef.kind )
        , ( "blend",
            case layerDef.layer of
                M.WebGLLayer _ webglBlend ->
                    WGLBlend.encodeOne webglBlend |> E.string
                M.SVGLayer _ svgBlend ->
                    SVGBlend.encode svgBlend |> E.string
          )
        , ( "blendDesc",
            case layerDef.layer of
                M.WebGLLayer _ webglBlend ->
                    webglBlend
                    |> WGLBlend.encodeHumanOne { delim = "; ", space = "> " }
                    |> E.string
                M.SVGLayer _ svgBlend ->
                    SVGBlend.encode svgBlend |> E.string
          )
        , ( "isOn", layerDef.on |> E.bool )
        , ( "model", encodeLayerModel layerDef.model )
        -- , ( "mesh", E.string "" )
        ]


encodeLayerModel : M.LayerModel -> E.Value
encodeLayerModel layerModel =
    E.object <|
        case layerModel of
            M.FssModel fssModel ->
                [ ( "renderMode", encodeFssRenderMode fssModel.renderMode |> E.string )
                , ( "faces", encodePairAsArray E.int fssModel.faces )
                , ( "lightSpeed", E.int fssModel.lightSpeed )
                , ( "amplitude", encodeTripleAsArray E.float fssModel.amplitude )
                , ( "mirror", E.bool fssModel.mirror )
                , ( "clip",
                        Maybe.withDefault FSS.noClip fssModel.clip
                        |> encodePairAsArray E.float
                  )
                ]
            _ -> []



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


encodePortModel : M.Model -> M.PortModel
encodePortModel model =
    { now = model.now
    , theta = model.theta
    , layers = List.map encodePortLayer model.layers
    , size = model.size
    , origin = model.origin
    , mouse = model.mouse
    , palette = model.product |> getPalette
    , product = model.product |> Product.encode
    }


encodePortLayer : M.LayerDef -> M.PortLayerDef
encodePortLayer layerDef =
    { kind = encodeKind_ layerDef.kind
    , on = layerDef.on
    , webglOrSvg =
        case layerDef.layer of
            M.WebGLLayer _ _ -> "webgl"
            M.SVGLayer _ _ -> "svg"
    , blend =
        case layerDef.layer of
            M.WebGLLayer _ webglBlend ->
                ( Just webglBlend, Nothing )
            M.SVGLayer _ svgBlend ->
                ( Nothing, SVGBlend.encode svgBlend |> Just )
    }


decodeKind : String -> M.LayerKind
decodeKind layerTypeStr =
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
        _ -> M.Empty


intPairDecoder : D.Decoder (Int, Int)
intPairDecoder =
    D.decode (\i1 i2 -> (i1, i2))
        |> D.required "v1" D.int
        |> D.required "v2" D.int

-- layerDefsDecoder =
--     let
--         createLayerDef kind blend isOn =
--             { kind = determineLayerType kind
--             , blend = WGLBlend.decodeOne blend
--                 |> Debug.log "Blend: "
--                 |> Maybe.withDefault WGLBlend.default
--             , isOn = isOn
--             --, config = FSS.init
--             -- , mesh = FSS.emptyMesh
--             }
--     in
--         D.list
--             ( D.decode createLayerDef
--                 |> D.required "kind" D.string
--                 |> D.required "blend" D.string
--                 |> D.required "isOn" D.bool
--             )


layerDefDecoder : D.Decoder M.LayerDef
layerDefDecoder =
    let
        createLayerDef kindStr renderType layerStr layerModelStr isOn =
            let kind = decodeKind kindStr
            in
                { kind = kind
                , on = isOn
                , layer =
                    layerStr
                        |> D.decodeString (layerDecoder kind renderType)
                        |> Result.toMaybe
                        |> Maybe.withDefault M.emptyLayer
                , model =
                    layerModelStr
                        |> D.decodeString (layerModelDecoder kind)
                        |> Result.toMaybe
                        |> Maybe.withDefault M.NoModel
                }
    in
        D.decode createLayerDef
            |> D.required "kind" D.string
            |> D.required "render-type" D.string
            |> D.required "layer" D.string
            |> D.required "model" D.string
            |> D.required "isOn" D.bool



layerDecoder : M.LayerKind -> String -> D.Decoder M.Layer
layerDecoder kind renderType =
    case kind of
        M.Fss ->
            let
                createLayer renderType model isOn =
                     -- TODO
                    M.SVGLayer M.NoContent SVGBlend.default
            in
                D.decode createLayer
                    |> D.required "render-type" D.string
                    |> D.required "model" D.string
                    |> D.required "isOn" D.bool
         -- TODO
        _ ->
            M.SVGLayer M.NoContent SVGBlend.default
                |> D.decode


layerModelDecoder : M.LayerKind -> D.Decoder M.LayerModel
layerModelDecoder kind =
    case kind of
        M.Fss ->
            let
                createLayer renderType model isOn =
                     -- TODO
                    M.NoModel
            in
                D.decode createLayer
                    |> D.required "render-type" D.string
                    |> D.required "model" D.string
                    |> D.required "isOn" D.bool
         -- TODO
        _ ->
            M.NoModel |> D.decode



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
            |> D.required "layers" (D.list layerDefDecoder)
            |> D.required "size" intPairDecoder
            |> D.required "origin" intPairDecoder
            |> D.required "mouse" intPairDecoder
            |> D.required "now" D.float


decodeModel : M.CreateLayer -> EncodedState -> Maybe M.Model
decodeModel createLayer modelStr =
    D.decodeString (modelDecoder createLayer) modelStr
        -- |> Debug.log "Decode Result: "
        |> Result.toMaybe


encodeFss : FSS.Model -> Product -> M.PortFSS
encodeFss m product =
    { amplitude = m.amplitude
    , faces = m.faces
    , lightSpeed = m.lightSpeed
    , renderMode = encodeFssRenderMode m.renderMode
    --, palette = product |> getPalette
    }


encodeFssRenderMode : FSS.RenderMode -> String
encodeFssRenderMode mode =
    case mode of
        FSS.Triangles -> "triangles"
        FSS.Lines -> "lines"
        FSS.PartialLines -> "partial-lines"
        FSS.Points -> "points"


decodeFssRenderMode : String -> FSS.RenderMode
decodeFssRenderMode str =
    case str of
        "triangles" -> FSS.Triangles
        "lines" -> FSS.Lines
        "partial-lines" -> FSS.PartialLines
        "points" -> FSS.Points
        _ -> FSS.Triangles


fromFssPortModel : FSS.PortModel -> FSS.Model
fromFssPortModel portModel =
    { portModel
    | renderMode = decodeFssRenderMode portModel.renderMode
    }
