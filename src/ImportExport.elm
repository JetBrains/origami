module ImportExport exposing
    ( encodeModel
    , decodeModel
    , encodePortModel
    , encodePortLayer
    , decodePortModel
    , encodeFss
    , fromFssPortModel
    )

import Array

import Tuple
import Time

import Json.Decode as D exposing (bool, int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (required, optional, hardcoded)
import Json.Decode.Extra as D exposing (andMap)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import WebGL.Blend as WGLBlend
import Html.Blend as HtmlBlend

import Layer.FSS as FSS
import Product exposing (..)

import Model as M
import TronGui as GUI


encodeIntPair : ( Int, Int ) -> E.Value
encodeIntPair ( v1, v2 ) =
    E.object
        [ ( "v1", E.int v1 )
        , ( "v2", E.int v2 )
        ]


-- encodePairAsArray : (a -> E.Value) -> ( a, a ) -> E.Value
-- encodePairAsArray f ( v1, v2 ) =
--     E.list f [ v1, v2 ]


encodeXY : (a -> E.Value) -> { x: a, y: a } -> E.Value
encodeXY f { x, y } =
    E.list
        f
        [ x
        , y
        ]


encodeColor : { r: Float, g: Float, b: Float } -> E.Value
encodeColor { r, g, b } =
    E.list
        E.float
        [ r
        , g
        , b
        ]


encodeColorShift : FSS.ColorShift -> E.Value
encodeColorShift { hue, saturation, brightness } =
    E.list
        E.float
        [ hue
        , saturation
        , brightness
        ]


encodeAmplitude : FSS.Amplitude -> E.Value
encodeAmplitude { amplitudeX, amplitudeY, amplitudeZ } =
    E.list
        E.float
        [ amplitudeX
        , amplitudeY
        , amplitudeZ
        ]


-- encodeTripleAsArray : (a -> E.Value) -> Array a -> E.Value
-- encodeTripleAsArray f [ v1, v2, v3 ] =
--    [ v1, v2, v3 ]
--         |> List.map f
--         |> Array.fromList
--         |> E.array


encodeKind_ : M.LayerKind -> String
encodeKind_ kind =
    case kind of
        M.Fss -> "fss"
        M.MirroredFss -> "fss-mirror"
        M.Lorenz -> "lorenz"
        M.Fractal -> "fractal"
        M.Template -> "template"
        M.Canvas -> "canvas"
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
                M.HtmlLayer _ htmlBlend ->
                    HtmlBlend.encode htmlBlend |> E.string
          )
        , ( "blendDesc",
            case layerDef.layer of
                M.WebGLLayer _ webglBlend ->
                    webglBlend
                    |> WGLBlend.encodeHumanOne { delim = "; ", space = "> " }
                    |> E.string
                M.HtmlLayer _ htmlBlend ->
                    HtmlBlend.encode htmlBlend |> E.string
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
                , ( "faces", encodeXY E.int fssModel.faces )
                , ( "lightSpeed", E.int fssModel.lightSpeed )
                , ( "amplitude", encodeAmplitude fssModel.amplitude )
                , ( "colorShift", encodeColorShift fssModel.colorShift )
                , ( "opacity", E.float fssModel.opacity )
                , ( "mirror", E.bool fssModel.mirror )
                , ( "clip",
                        Maybe.withDefault FSS.noClip fssModel.clip
                            |> encodeXY E.float
                  )
                , ( "shareMesh", E.bool fssModel.shareMesh )
                , ( "vignette", E.float fssModel.vignette )
                , ( "iris", E.float fssModel.iris )
                ]
            M.VignetteModel vignetteModel ->
                [ ( "opacity", E.float vignetteModel.opacity )
                , ( "color", encodeColor vignetteModel.color )
                ]
            _ -> []


encodeModel_ : M.Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "background", E.string model.background )
        , ( "mode", E.string <| M.encodeMode model.mode )
        , ( "theta", E.float model.theta )
        , ( "omega", E.float model.omega )
        , ( "layers", E.list encodeLayerDef model.layers )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        -- for b/w compatibility, we also encode size as numbers, but sizeRule is what should matter
        -- when it is defined/known on import
        , ( "size", encodeIntPair <| M.getRuleSizeOrZeroes model.size )
        , ( "sizeRule", E.string <| M.encodeSizeRule model.size )
        , ( "origin", encodeIntPair model.origin )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "now", E.float model.now )
        , ( "palette",
            model.product
                |> getPalette
                |> E.list E.string )
        , ( "product", model.product |> Product.encode |> E.string )
        ]


encodeModel : M.Model -> String
encodeModel model = model |> encodeModel_ |> E.encode 2


encodePortModel : M.Model -> M.PortModel
encodePortModel model =
    { background = model.background
    , mode = M.encodeMode model.mode
    , now = model.now
    , theta = model.theta
    , omega = model.omega
    , layers = List.map encodePortLayer model.layers
    , size = M.getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
    , sizeRule = M.encodeSizeRule model.size |> Just
    , origin = model.origin
    , mouse = model.mouse
    , palette = model.product |> getPalette
    , product = model.product |> Product.encode
    }


decodePortModel : M.CreateLayer -> M.PortModel -> M.Model
decodePortModel createLayer portModel =
    let
        mode = M.decodeMode portModel.mode
        initialModel = M.initEmpty mode
        decodedModel =
            { initialModel
            | background = portModel.background
            , mode = mode
            , now = portModel.now
            , theta = portModel.theta
            , omega = portModel.omega
            , layers = List.map (decodePortLayer createLayer) portModel.layers
            , size =
                case portModel.sizeRule of
                    Just sizeRuleStr -> M.decodeSizeRule sizeRuleStr
                    Nothing -> case portModel.size of
                        ( w, h ) -> M.Custom w h
            , origin = portModel.origin
            , mouse = portModel.mouse
            , product = portModel.product |> Product.decode
            }
    in
        { decodedModel
        | gui = case mode of
            M.TronUi _ -> GUI.gui decodedModel |> Just
            _ -> Nothing
        }


encodePortLayer : M.LayerDef -> M.PortLayerDef
encodePortLayer layerDef =
    { kind = encodeKind_ layerDef.kind
    , isOn = layerDef.on
    , webglOrHtml =
        case layerDef.layer of
            M.WebGLLayer _ _ -> "webgl"
            M.HtmlLayer _ _ -> "html"
    , blend =
        case layerDef.layer of
            M.WebGLLayer _ webglBlend ->
                ( Just webglBlend, Nothing )
            M.HtmlLayer _ htmlBlend ->
                ( Nothing, HtmlBlend.encode htmlBlend |> Just )
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
            M.HtmlLayer htmlLayer _ ->
                portLayerDef.blend
                    |> Tuple.second
                    |> Maybe.map HtmlBlend.decode
                    |> Maybe.withDefault HtmlBlend.default
                    |> M.HtmlLayer htmlLayer
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
    D.map2 Tuple.pair
        (D.field "v1" D.int)
        (D.field "v2" D.int)

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
                    M.HtmlLayer htmlLayer _ ->
                        HtmlBlend.decode blendStr |>
                            M.HtmlLayer htmlLayer
            in
                { kind = kind
                , on = isOn
                , layer = layer
                , model = layerModel
                , name = name
                }
    in
        D.map5 createLayerDef
            (D.field "kind" D.string)
            (D.field "model" D.string)
            (D.field "name" D.string)
            (D.field "isOn" D.bool)
            (D.field "blend" D.string)


-- layerDecoder : M.LayerKind -> D.Decoder M.Layer
-- layerDecoder kind =
--     case kind of
--         M.Fss ->
--             let
--                 createLayer renderType model isOn =
--                      -- TODO
--                     M.HtmlLayer M.NoContent HtmlBlend.default
--             in
--                 D.decode createLayer
--                     |> D.required "renderMode" D.string
--                     |> D.required "model" D.string
--                     |> D.required "isOn" D.bool
--          -- TODO
--         _ ->
--             M.HtmlLayer M.NoContent HtmlBlend.default
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
                    case [ faces, amplitude, colorShift, clip ] of
                        [ [facesX, facesY], [amplitudeX, amplitudeY, amplitudeZ], [hue, saturation, brightness], [clipX, clipY] ] ->
                            M.FssModel
                                { renderMode = FSS.decodeRenderMode renderModeStr
                                , faces = { x = floor facesX, y = floor facesY }
                                , amplitude =
                                    { amplitudeX = amplitudeX
                                    , amplitudeY = amplitudeY
                                    , amplitudeZ = amplitudeZ
                                    }
                                , colorShift =
                                    { hue = hue
                                    , saturation = saturation
                                    , brightness = brightness
                                    }
                                , opacity = opacity
                                , mirror = mirror
                                , clip = Just { x = clipX, y = clipY }
                                , lightSpeed = lightSpeed
                                , shareMesh = shareMesh
                                , vignette = vignette
                                , iris = iris
                                }
                        _ -> M.NoModel
                        -- _ -> Debug.log "failed to parse model" M.NoModel
            in
                D.succeed createFssModel
                    |> D.andMap (D.field "renderMode" D.string)
                    |> D.andMap (D.field "faces" <| D.list D.float)
                    |> D.andMap (D.field "amplitude" <| D.list D.float)
                    |> D.andMap (D.field "colorShift" <| D.list D.float)
                    |> D.andMap (D.field "opacity" D.float)
                    |> D.andMap (D.field "mirror" D.bool)
                    |> D.andMap (D.field "clip" <| D.list D.float)
                    |> D.andMap (D.field "lightSpeed" D.int)
                    |> D.andMap (D.field "shareMesh" D.bool)
                    |> D.andMap (D.field "vignette" D.float)
                    |> D.andMap (D.field "iris" D.float)
        M.MirroredFss ->
            layerModelDecoder M.Fss
        -- TODO
        _ ->
            D.succeed M.NoModel


modelDecoder : M.UiMode -> M.CreateLayer -> M.CreateGui -> D.Decoder M.Model
modelDecoder mode createLayer createGui =
    let
        createModel
            background
            theta
            omega
            layers
            maybeSize
            maybeSizeRule
            origin
            mouse
            now
            productStr =
            let
                initialModel = M.init mode [] createLayer createGui
                product = Product.decode productStr
            in
                { initialModel
                | background = background
                , theta = theta
                , omega = omega
                , layers = layers
                , size = case maybeSizeRule of
                    Just sizeRuleStr -> M.decodeSizeRule sizeRuleStr
                    Nothing -> case maybeSize of
                        Just (w, h) -> M.Custom w h
                        Nothing -> M.Dimensionless
                , origin = origin
                , mouse = mouse
                , now = now
                , product = product
                --, palette = Product.getPalette product
                }
    in
        D.succeed createModel
            |> D.andMap (D.field "background" D.string)
            |> D.andMap (D.field "theta" D.float)
            |> D.andMap (D.field "omega" D.float)
            |> D.andMap (D.field "layers" (layerDefDecoder createLayer |> D.list))
            |> D.andMap (D.maybe (D.field "size" intPairDecoder))
            |> D.andMap (D.maybe (D.field "sizeRule" D.string))
            |> D.andMap (D.field "origin" intPairDecoder)
            |> D.andMap (D.field "mouse" intPairDecoder)
            |> D.andMap (D.field "now" D.float)
            |> D.andMap (D.field "product" D.string)


decodeModel : M.UiMode -> M.CreateLayer -> M.CreateGui -> String -> Maybe M.Model
decodeModel mode createLayer createGui modelStr =
    D.decodeString (modelDecoder mode createLayer createGui) modelStr
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
fromFssPortModel pm =
    { amplitude = pm.amplitude
    , colorShift = pm.colorShift
    , opacity = pm.opacity
    , faces = pm.faces
    , lightSpeed = pm.lightSpeed
    , renderMode = FSS.decodeRenderMode pm.renderMode
    , clip = pm.clip
    , shareMesh = pm.shareMesh
    , vignette = pm.vignette
    , iris = pm.iris
    , mirror = pm.mirror
    --, palette = product |> getPalette
    }
