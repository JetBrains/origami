module ImportExport exposing
    ( encodeModel
    , decodeModel
    , encodePortModel
    , encodePortLayer
    , decodePortModel
    , encodeFss
    , fromFssPortModel
    , encodeMode
    , decodeMode
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
   [ v1, v2, v3 ]
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
        M.Cover -> "cover"
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
        , ( "name", layerDef.name |> E.string )
        -- , ( "mesh", E.string "" )
        ]


encodeLayerModel : M.LayerModel -> E.Value
encodeLayerModel layerModel =
    E.object <|
        case layerModel of
            M.FssModel fssModel ->
                [ ( "renderMode", FSS.encodeRenderMode fssModel.renderMode |> E.string )
                , ( "faces", encodePairAsArray E.int fssModel.faces )
                , ( "lightSpeed", E.int fssModel.lightSpeed )
                , ( "amplitude", encodeTripleAsArray E.float fssModel.amplitude )
                , ( "colorShift", encodeTripleAsArray E.float fssModel.colorShift )
                , ( "opacity", E.float fssModel.opacity )
                , ( "mirror", E.bool fssModel.mirror )
                , ( "clip",
                        Maybe.withDefault FSS.noClip fssModel.clip
                        |> encodePairAsArray E.float
                  )
                , ( "shareMesh", E.bool fssModel.shareMesh )
                , ( "vignette", E.float fssModel.vignette )
                , ( "iris", E.float fssModel.iris )
                ]
            M.VignetteModel vignetteModel ->
                [ ( "opacity", E.float vignetteModel.opacity )
                , ( "color", encodeTripleAsArray E.float vignetteModel.color )
                ]
            _ -> []


encodeModel_ : M.Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "background", E.string model.background )
        , ( "mode", E.string <| encodeMode model.mode )
        , ( "theta", E.float model.theta )
        , ( "omega", E.float model.omega )
        , ( "layers", E.list (List.map encodeLayerDef model.layers) )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        , ( "size", encodeIntPair model.size )
        , ( "origin", encodeIntPair model.origin )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "now", E.float model.now )
        , ( "palette",
            model.product
                |> getPalette
                |> List.map E.string
                |> Array.fromList
                |> E.array )
        , ( "product", model.product |> Product.encode |> E.string )
        ]


encodeModel : M.Model -> String
encodeModel model = model |> encodeModel_ |> E.encode 2


encodePortModel : M.Model -> M.PortModel
encodePortModel model =
    { background = model.background
    , mode = encodeMode model.mode
    , now = model.now
    , theta = model.theta
    , omega = model.omega
    , layers = List.map encodePortLayer model.layers
    , size = model.size
    , origin = model.origin
    , mouse = model.mouse
    , palette = model.product |> getPalette
    , product = model.product |> Product.encode
    }


decodePortModel : M.CreateLayer -> M.PortModel -> M.Model
decodePortModel createLayer portModel =
    let
        mode = decodeMode portModel.mode
        initialModel = M.initEmpty mode
        decodedModel =
            { initialModel
            | background = portModel.background
            , mode = mode
            , now = portModel.now
            , theta = portModel.theta
            , omega = portModel.omega
            , layers = List.map (decodePortLayer createLayer) portModel.layers
            , size = portModel.size
            , origin = portModel.origin
            , mouse = portModel.mouse
            , product = portModel.product |> Product.decode
            }
    in
        { decodedModel
        | gui = case mode of
            M.TronUi _ -> M.gui decodedModel |> Just
            _ -> Nothing
        }


encodePortLayer : M.LayerDef -> M.PortLayerDef
encodePortLayer layerDef =
    { kind = encodeKind_ layerDef.kind
    , isOn = layerDef.on
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
    , name = layerDef.name
    , model = layerDef.model
        |> encodeLayerModel
        |> E.encode 2
    }


decodePortLayer : M.CreateLayer -> M.PortLayerDef -> M.LayerDef
decodePortLayer createLayer portLayerDef =
    let
        kind = decodeKind portLayerDef.kind
        layerModel = portLayerDef.model
                |> D.decodeString (layerModelDecoder kind)
                -- |> Debug.log "Layer Model Decode Result: "
                |> Result.toMaybe
                |> Maybe.withDefault M.NoModel
        layerNoBlend = createLayer kind layerModel
        layer = case layerNoBlend of
            M.WebGLLayer webglLayer _ ->
                portLayerDef.blend
                    |> Tuple.first
                    |> Maybe.withDefault WGLBlend.default
                    |> M.WebGLLayer webglLayer
            M.SVGLayer svgLayer _ ->
                portLayerDef.blend
                    |> Tuple.second
                    |> Maybe.map SVGBlend.decode
                    |> Maybe.withDefault SVGBlend.default
                    |> M.SVGLayer svgLayer
    in
        { kind = kind
        , on = portLayerDef.isOn
        , layer = layer
        , model = layerModel
        , name = portLayerDef.name
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
        "cover" -> M.Cover
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


layerDefDecoder : M.CreateLayer -> D.Decoder M.LayerDef
layerDefDecoder createLayer =
    let
        createLayerDef kindStr layerModelStr name isOn blendStr =
            let
                kind = decodeKind kindStr
                layerModel = layerModelStr
                        |> D.decodeString (layerModelDecoder kind)
                        -- |> Debug.log "Layer Model Decode Result: "
                        |> Result.toMaybe
                        |> Maybe.withDefault M.NoModel
                layerNoBlend = createLayer kind layerModel
                layer = case layerNoBlend of
                    M.WebGLLayer webglLayer _ ->
                        WGLBlend.decodeOne blendStr
                            |> Maybe.withDefault WGLBlend.default
                            |> M.WebGLLayer webglLayer
                    M.SVGLayer svgLayer _ ->
                        SVGBlend.decode blendStr |>
                            M.SVGLayer svgLayer
            in
                { kind = kind
                , on = isOn
                , layer = layer
                , model = layerModel
                , name = name
                }
    in
        D.decode createLayerDef
            |> D.required "kind" D.string
            |> D.required "model" D.string
            |> D.required "name" D.string
            |> D.required "isOn" D.bool
            |> D.required "blend" D.string


-- layerDecoder : M.LayerKind -> D.Decoder M.Layer
-- layerDecoder kind =
--     case kind of
--         M.Fss ->
--             let
--                 createLayer renderType model isOn =
--                      -- TODO
--                     M.SVGLayer M.NoContent SVGBlend.default
--             in
--                 D.decode createLayer
--                     |> D.required "renderMode" D.string
--                     |> D.required "model" D.string
--                     |> D.required "isOn" D.bool
--          -- TODO
--         _ ->
--             M.SVGLayer M.NoContent SVGBlend.default
--                 |> D.decode


layerModelDecoder : M.LayerKind -> D.Decoder M.LayerModel
layerModelDecoder kind =
    case kind of
        M.Fss ->
            let
                createFssModel
                    renderModeStr
                    faces
                    amplitude
                    colorShift
                    opacity
                    mirror
                    clip
                    lightSpeed
                    shareMesh
                    vignette
                    iris =
                case ( faces, amplitude, colorShift, opacity, clip ) of
                    ( [facesX, facesY]
                    , [amplitudeX, amplitudeY, amplitudeZ]
                    , [hue, saturation, brightness]
                    , opacity
                    , [clipX, clipY]
                    ) ->
                        M.FssModel
                            { renderMode = FSS.decodeRenderMode renderModeStr
                            , faces = ( facesX, facesY )
                            , amplitude = ( amplitudeX, amplitudeY, amplitudeZ )
                            , colorShift = ( hue, saturation, brightness )
                            , opacity = opacity
                            , mirror = mirror
                            , clip = Just ( clipX, clipY )
                            , lightSpeed = lightSpeed
                            , shareMesh = shareMesh
                            , vignette = vignette
                            , iris = iris
                            }
                    _ -> M.NoModel
                    -- _ -> Debug.log "failed to parse model" M.NoModel
            in
                D.decode createFssModel
                    |> D.required "renderMode" D.string
                    |> D.required "faces" (D.list D.int)
                    |> D.required "amplitude" (D.list D.float)
                    |> D.required "colorShift" (D.list D.float)
                    |> D.required "opacity" D.float
                    |> D.required "mirror" D.bool
                    |> D.required "clip" (D.list D.float)
                    |> D.required "lightSpeed" D.int
                    |> D.required "shareMesh" D.bool
                    |> D.required "vignette" D.float
                    |> D.required "iris" D.float
        M.MirroredFss ->
            layerModelDecoder M.Fss
        -- TODO
        _ ->
            M.NoModel |> D.decode


modelDecoder : M.UiMode -> M.CreateLayer -> D.Decoder M.Model
modelDecoder mode createLayer =
    let
        createModel background theta omega layers size origin mouse now productStr =
            let
                initialModel = M.init mode [] createLayer
                product = Product.decode productStr
            in
                { initialModel
                | background = background
                , theta = theta
                , omega = omega
                , layers = layers
                , size = size
                , origin = origin
                , mouse = mouse
                , now = now
                , product = product
                --, palette = Product.getPalette product
                }
    in
        D.decode createModel
            |> D.required "background" D.string
            |> D.required "theta" D.float
            |> D.required "omega" D.float
            |> D.required "layers" (layerDefDecoder createLayer |> D.list)
            |> D.required "size" intPairDecoder
            |> D.required "origin" intPairDecoder
            |> D.required "mouse" intPairDecoder
            |> D.required "now" D.float
            |> D.required "product" D.string


decodeModel : M.UiMode -> M.CreateLayer -> String -> Maybe M.Model
decodeModel mode createLayer modelStr =
    D.decodeString (modelDecoder mode createLayer) modelStr
        -- |> Debug.log "Decode Result: "
        |> Result.toMaybe


encodeFss : FSS.Model -> Product -> FSS.PortModel
encodeFss m product =
    { amplitude = m.amplitude
    , colorShift = m.colorShift
    , opacity = m.opacity
    , faces = m.faces
    , lightSpeed = m.lightSpeed
    , renderMode = FSS.encodeRenderMode m.renderMode
    , clip = m.clip
    , shareMesh = m.shareMesh
    , vignette = m.vignette
    , iris = m.iris
    , mirror = m.mirror
    --, palette = product |> getPalette
    }


fromFssPortModel : FSS.PortModel -> FSS.Model
fromFssPortModel portModel =
    { portModel
    | renderMode = FSS.decodeRenderMode portModel.renderMode
    }


encodeMode : M.UiMode -> String
encodeMode mode =
    case mode of
        M.Development -> "dev"
        M.Production -> "prod"
        M.Release -> "release"
        M.Ads -> "ads"
        M.TronUi innerMode -> "tron-" ++ encodeMode innerMode


decodeMode : String -> M.UiMode
decodeMode mode =
    if String.startsWith mode "tron-"
    then decodeMode <| String.dropRight 5 mode
    else
        case mode of
            "dev" -> M.Development
            "prod" -> M.Production
            "release" -> M.Release
            "ads" -> M.Ads
            _ -> M.Production
