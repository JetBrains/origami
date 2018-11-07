port module Main exposing (main)

import Either exposing (Either)
import Array exposing (Array)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
    exposing (class, width, height, style, class, type_, min, max, value, id)
import Html.Events exposing (on, onInput, onMouseUp, onClick)
import AnimationFrame
import Time exposing (Time)
import Window
import Mouse exposing (clicks, moves, Position)
import Task exposing (Task)
import WebGL exposing (Mesh, Option)
import WebGL.Settings.Blend as B
import WebGL.Settings exposing (sampleAlphaToCoverage)
import WebGL.Settings.DepthTest as DepthTest

import Viewport exposing (Viewport)
import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend
import Controls
import ImportExport as IE exposing (EncodedState)
import Product exposing (Product)
import Product as Product

import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.FSS as FSS
import Layer.Template as Template
import Layer.JbText as JbText
import Layer.SVGImage as SVGImage
import Layer.Vignette as Vignette


type alias LayerIndex = Int

type alias Size = (Int, Int)
type alias Pos = (Int, Int)

type alias ModelChange = LayerModel -> LayerModel

type LayerKind
    = Lorenz
    | Fractal
    | Template
    | Voronoi
    | Fss
    | MirroredFss
    | Text
    | SvgImage
    | Vignette


-- type LayerBlend
--     = WGLB WGLBlend.Blend
--     | SVGB SVGBlend.Blend


type LayerModel
    = LorenzModel Lorenz.Model
    | FractalModel Fractal.Model
    | VoronoiModel Voronoi.Model
    | FssModel FSS.Model
    | TemplateModel Template.Model
    | NoModel


type WebGLLayer
    = LorenzLayer Lorenz.Mesh
    | FractalLayer Fractal.Mesh
    | VoronoiLayer Voronoi.Mesh
    | TemplateLayer Template.Mesh
    | FssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | MirroredFssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | VignetteLayer

type SVGLayer
    = TextLayer
    | SvgImageLayer
    -- | CanvasLayer (\_ -> )

type alias Layer =
    Either
        ( WebGLLayer, WGLBlend.Blend )
        ( SVGLayer, SVGBlend.Blend )

-- `change` is needed since we store a sample layer model
-- to use for any layer in the main model
type alias LayerDef =
    { kind: LayerKind
    , layer: Layer
    , change: ModelChange
    , on: Bool
    }


initialLayers : List ( LayerKind, ModelChange )
initialLayers =
    [ ( MirroredFss, identity )
    , ( MirroredFss, identity )
    , ( MirroredFss
      , changeIfFss
            (\prevModel ->
                { prevModel
                | renderMode = FSS.PartialLines
                , shareMesh = True
                }
            )
      )
    , ( Vignette, identity )
    , ( Text, identity )
    , ( SvgImage, identity )
    ]


type alias Model =
    { paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , layers : List LayerDef
    , size : Size
    , origin : Pos
    , mouse : (Int, Int)
    , now : Time
    , timeShift : Time
    , product : Product
    , vignette : Vignette.Model
    , fss : FSS.Model
    , lorenz : Lorenz.Model
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }


type alias FssBuildOptions = GuiConfig

type alias GuiConfig =
    { product : String
    , palette : List String
    , layers : List
        { kind: String
        , blend : IE.PortBlend
        , webglOrSvg: String
        , on: Bool
        }
    , size : ( Int, Int )
    , facesX : Int
    , facesY : Int
    , lightSpeed: Int
    , vignette: Float
    , amplitude : FSS.AmplitudeChange
    , customSize : Maybe (Int, Int)
    }


type Msg
    = Bang
    | Animate Time
    | Resize Window.Size
    | Configure LayerIndex LayerModel
    | TurnOn LayerIndex
    | TurnOff LayerIndex
    | ChangeWGLBlend LayerIndex WGLBlend.Blend
    | ChangeSVGBlend LayerIndex SVGBlend.Blend
    | RebuildFss LayerIndex FSS.SerializedScene
    --| RebuildOnClient LayerIndex FSS.SerializedScene
    | Rotate Float
    | ChangeFacesX Int
    | ChangeFacesY Int
    | ChangeLightSpeed Int
    | ChangeVignette Float
    | ChangeAmplitude FSS.AmplitudeChange
    | ChangeProduct Product
    | Locate Position
    | Import EncodedState
    | Export
    | ExportZip
    | TimeTravel Float
    | BackToNow
    | Pause
    | Continue
    | TriggerPause
    | NoOp


sizeCoef : Float
sizeCoef = 1.0


init : ( Model, Cmd Msg )
init =
    ( { paused = False
      , autoRotate = False
      , fps = 0
      , theta = 0.1
      , layers = initialLayers |> List.map
            (\(kind, change) ->
                { kind = kind
                , layer = createLayer kind change
                , change = change
                , on = True
                })
      , size = ( 1200, 1200 )
      , origin = ( 0, 0 )
      , mouse = ( 0, 0 )
      , now = 0.0
      , timeShift = 0.0
      --, range = ( 0.8, 1.0 )
      , product = Product.JetBrains
      , fss = FSS.init
      , vignette = Vignette.init
      , lorenz = Lorenz.init
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )


updateAndRebuildFssWith : Model -> ( Model, Cmd Msg )
updateAndRebuildFssWith model =
    ( model, model |> extractFssBuildOptions |> requestFssRebuild )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ fss, vignette } as model) =
    case msg of

        Bang ->
            ( model
            , model |> prepareGuiConfig |> startGui
            )

        Animate dt ->
            (
                { model
                 | fps = floor (1000 / dt)
                 , theta = if not model.autoRotate
                              then model.theta + dt / 4000
                              else model.theta
                 , now = if not model.paused
                            then model.now + dt + model.timeShift
                            else model.now
                 }
            , Cmd.none
            )

        Configure index layerModel ->
            ( model |> updateLayer index
                (\webglLayer change ->
                    case ( webglLayer, change layerModel ) of
                        ( LorenzLayer _, LorenzModel lorenzModel ) ->
                            LorenzLayer (lorenzModel |> Lorenz.build)
                        ( FractalLayer _, FractalModel fractalModel ) ->
                            FractalLayer (fractalModel |> Fractal.build)
                        ( VoronoiLayer _, VoronoiModel voronoiModel ) ->
                            VoronoiLayer (voronoiModel |> Voronoi.build)
                        ( FssLayer maybeScene _, FssModel fssModel ) ->
                            let
                                newMesh = maybeScene |> FSS.build fssModel
                            in
                                FssLayer maybeScene newMesh
                        ( MirroredFssLayer maybeScene _, FssModel fssModel ) ->
                            let
                                newMesh = maybeScene |> FSS.build fssModel
                            in
                                MirroredFssLayer maybeScene newMesh
                        ( TemplateLayer _, TemplateModel templateModel ) ->
                            TemplateLayer (templateModel |> Template.build)
                        _ -> webglLayer)
                (\svgLayer _ -> svgLayer)
            , Cmd.none
            )

        ChangeWGLBlend index newBlend ->
            ( model |> updateLayerBlend index
                (\_ -> Just newBlend)
                (\_ -> Nothing)
            , Cmd.none
            )

        ChangeSVGBlend index newBlend ->
            ( model |> updateLayerBlend index
                (\_ -> Nothing)
                (\_ -> Just newBlend)
            , Cmd.none
            )

        ChangeFacesX facesX ->
            updateAndRebuildFssWith
                { model | fss =
                    { fss | faces = fss.faces |> Tuple.mapFirst (\_ -> facesX) }
                }

        ChangeFacesY facesY ->
            updateAndRebuildFssWith
                { model | fss =
                    { fss | faces = fss.faces |> Tuple.mapSecond (\_ -> facesY) }
                }

        ChangeLightSpeed lightSpeed ->
            updateAndRebuildFssWith
                { model | fss =
                    { fss | lightSpeed = lightSpeed }
                }

        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )

        Resize { width, height } ->
            ( { model
              | size = adaptSize ( width, height )
              , origin = getOrigin ( width, height )
              }
            -- , model |> extractFssBuildOptions |> requestFssRebuild
            , Cmd.none
            )

        Locate pos ->
            ( { model | mouse = (pos.x, pos.y) }
            , Cmd.none
            )

        RebuildFss index serializedScene ->
            ( model |> updateLayer index
                (\webglLayer change ->
                    case webglLayer of
                        FssLayer _ mesh ->
                            let
                                maybeScene = Just serializedScene
                                newMesh = maybeScene
                                    |> FSS.build (applyFssChange change model.fss)
                            in
                                FssLayer maybeScene newMesh
                        MirroredFssLayer _ mesh ->
                            let
                                maybeScene = Just serializedScene
                                newMesh = maybeScene
                                    |> FSS.build (applyFssChange change model.fss)
                            in
                                MirroredFssLayer maybeScene newMesh
                        _ -> webglLayer
                )
                (\svgLayer _ -> svgLayer)
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )

        Continue ->
            ( { model | paused = False }
            , Cmd.none
            )

        TriggerPause ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        Import encodedModel ->
            ( IE.decodeModel encodedModel
                (\src ->
                    { model
                    | theta = src.theta
                    , now = src.now
                    , layers =
                        src.layers
                            |> List.map2 extractLayer model.layers
                    , mouse = src.mouse
                    , size = src.size
                    , origin = src.origin
                    -- , size = adaptSize src.size
                    -- , origin = getOrigin src.size
                    } )
                -- |> Debug.log "decoded model"
                |> Maybe.withDefault model
            , Cmd.none )

        Export ->
            ( model
            , model |> prepareModel |> IE.encodeModel |> export_
            )

        ExportZip ->
            ( model
            , model |> prepareModel |> IE.encodeModel |> exportZip_
            )

        TimeTravel timeShift ->
            (
                { model
                | timeShift = timeShift
                }
            , Cmd.none )

        BackToNow ->
            ( { model | timeShift = 0.0 }
            , Cmd.none
            )

        ChangeProduct product ->
            updateAndRebuildFssWith
                { model | product = product }

        ChangeVignette opacity ->
            ( { model | vignette =
                { vignette | opacity = opacity }
            }
            , Cmd.none
            )

        ChangeAmplitude ( newAmplitudeX, newAmplitudeY, newAmplitudeZ ) ->
            let
                ( currentAmplitudeX, currentAmplitudeY, currentAmplitudeZ )
                    = fss.amplitude
            in
                ( { model | fss =
                    { fss | amplitude =
                        ( Maybe.withDefault currentAmplitudeX newAmplitudeX
                        , Maybe.withDefault currentAmplitudeY newAmplitudeY
                        , Maybe.withDefault currentAmplitudeZ newAmplitudeZ
                        )
                    }
                  }
                , Cmd.none
                )

        TurnOn index ->
            ( model |> updateLayerDef index
                (\def -> { def | on = True })
            , Cmd.none
            )

        TurnOff index ->
            ( model |> updateLayerDef index
                (\def -> { def | on = False })
            , Cmd.none
            )

        NoOp -> ( model, Cmd.none )


-- getLayerKind : Layer -> LayerKind
-- getLayerKind layer =
--     case layer of
--         FssLayer _ _ -> Fss
--         MirroredFssLayer _ _ -> MirroredFss
--         LorenzLayer _ -> Lorenz
--         FractalLayer _ -> Fractal
--         VoronoiLayer _ -> Voronoi
--         TemplateLayer _ -> Template
--         TextLayer -> Text
--         SvgImageLayer -> SvgImage
--         VignetteLayer -> Vignette
--         _ -> Text -- FIXME: Empty Kind or Nothing


-- getBlendString : Layer -> String
-- getBlendString layer =
--     case layer of
--         FssLayer blend _ _ -> WGLBlend.encodeOne blend
--         MirroredFssLayer blend _ _ -> WGLBlend.encodeOne blend
--         LorenzLayer blend _ -> WGLBlend.encodeOne blend
--         FractalLayer blend _ -> WGLBlend.encodeOne blend
--         VoronoiLayer blend _ -> WGLBlend.encodeOne blend
--         TemplateLayer blend _ -> WGLBlend.encodeOne blend
--         VignetteLayer blend -> WGLBlend.encodeOne blend
--         TextLayer blend -> SVGBlend.encode blend
--         SvgImageLayer blend -> SVGBlend.encode blend
--         _ -> SVGBlend.encode SVGBlend.default


getBlendForPort : Layer -> IE.PortBlend
getBlendForPort layer =
    ( case layer of
        Either.Left ( _, webglBlend ) -> Just webglBlend
        Either.Right _ -> Nothing
    , case layer of
        Either.Right ( _, svgBlend ) ->
            SVGBlend.encode svgBlend |> Just
        Either.Left _ -> Nothing
    )


encodeLayerKind : LayerKind -> String
encodeLayerKind kind =
    case kind of
        Fss -> "fss"
        MirroredFss -> "fss-mirror"
        Lorenz -> "lorenz"
        Template -> "template"
        Voronoi -> "voronoi"
        Fractal -> "fractal"
        Text -> "text"
        SvgImage -> "svg"
        Vignette -> "vignette"


-- decodeLayerKind : String -> Maybe LayerKind
-- decodeLayerKind code =
--     case code of
--         "fss" -> Just (Fss Nothing) -- FIXME: not Nothing
--         "lorenz" -> Just Lorenz
--         "template" -> Just Template
--         "voronoi" -> Just Voronoi
--         "fractal" -> Just Fractal
--         "text" -> Just Text
--         "svg" -> Just SvgImage
--         "vignette" -> Just Vignette
--         _ -> Nothing


createLayer : LayerKind -> ModelChange -> Layer
createLayer kind change =
    case kind of
        Fss ->
            let
                config =
                    applyFssChange change FSS.init
            in
                ( FSS.build config Nothing |> FssLayer Nothing
                , WGLBlend.default )
                |> Either.Left
        MirroredFss ->
            let
                config =
                    applyFssChange change FSS.init
            in
                ( FSS.build config Nothing |> MirroredFssLayer Nothing
                , WGLBlend.default )
                -- (WGLBlend.build
                --    (B.customAdd, B.oneMinusSrcColor, B.oneMinusSrcColor)
                --    (B.customAdd, B.srcColor, B.zero)
                -- )
                |> Either.Left
        Lorenz ->
            let
                config =
                    case LorenzModel Lorenz.init |> change of
                        LorenzModel newModel -> newModel
                        _ -> Lorenz.init
            in
                ( Lorenz.build config |> LorenzLayer
                , WGLBlend.default )
                |> Either.Left
        Template ->
            let
                config =
                    case TemplateModel Template.init |> change of
                        TemplateModel newModel -> newModel
                        _ -> Template.init
            in
                ( Template.build config |> TemplateLayer
                , WGLBlend.default )
                |> Either.Left
        Voronoi ->
            let
                config =
                    case VoronoiModel Voronoi.init |> change of
                        VoronoiModel newModel -> newModel
                        _ -> Template.init
            in
                ( Voronoi.build config |> VoronoiLayer
                , WGLBlend.default )
                |> Either.Left
        Fractal ->
            let
                config =
                    case FractalModel Fractal.init |> change of
                        FractalModel newModel -> newModel
                        _ -> Fractal.init
            in
                ( Fractal.build config |> FractalLayer
                , WGLBlend.default )
                |> Either.Left
        Vignette ->
            ( VignetteLayer
            , WGLBlend.build
                (B.customAdd, B.srcAlpha, B.oneMinusSrcAlpha)
                (B.customAdd, B.one, B.oneMinusSrcAlpha) )
            |> Either.Left
            -- WGLBlend.Blend Nothing (0, 1, 7) (0, 1, 7) |> VignetteLayer Vignette.init
            -- VignetteLayer Vignette.init WGLBlend.default
        Text ->
            ( TextLayer, SVGBlend.default )
            |> Either.Right
        SvgImage ->
            ( SvgImageLayer, SVGBlend.default )
            |> Either.Right


extractLayer : LayerDef -> IE.Layer -> LayerDef
extractLayer curLayer srcLayer =
    { kind = curLayer.kind
    , layer = curLayer.layer
        -- FIXME: TODO
        -- case ( srcLayer.type_, curLayer.kind ) of
        --     ( IE.Fss, Fss ) ->
        --         curLayer.layer
        --     ( IE.MirroredFss, MirroredFss ) ->
        --         curLayer.layer
        --     _ -> curLayer
    , change = curLayer.change
    , on = curLayer.on
    -- , blend = srcLayer.blend
    }



prepareLayer : LayerDef -> IE.Layer
prepareLayer { kind, layer, on } =
    case ( kind, layer ) of
        ( Fss, Either.Left ( _, blend ) ) ->
            { kind = IE.Fss
            , blend = blend
            , isOn = on
            }
        ( MirroredFss, Either.Left ( _, blend ) ) ->
            { kind = IE.MirroredFss
            , blend = blend
            , isOn = on
            }
        _ -> IE.defaultLayer


prepareModel : Model -> IE.Model
prepareModel model =
    { theta = model.theta
    , now = model.now
    , layers = model.layers
        |> List.map prepareLayer
    , mouse = model.mouse
    , size = model.size
    , origin = model.origin
    }


extractFssBuildOptions : Model -> FssBuildOptions
extractFssBuildOptions = prepareGuiConfig


prepareGuiConfig : Model -> GuiConfig
prepareGuiConfig ({ fss } as model) =
    let
        ( amplitudeX, amplitudeY, amplitudeZ ) = fss.amplitude
    in
        { product = Product.encode model.product
        , palette = Product.getPalette model.product
        , size = ( Tuple.first model.size |> toFloat |> (*) 1.8 |> floor
                 , Tuple.second model.size |> toFloat |> (*) 1.8 |> floor
                 )
        , layers =
            model.layers |>
                List.map (\{ kind, layer, on } ->
                    { kind = encodeLayerKind kind
                    , blend = getBlendForPort layer
                    , webglOrSvg = if isWebGLLayer layer then "webgl" else "svg"
                    , on = on
                    })
        , facesX = Tuple.first model.fss.faces
        , facesY = Tuple.second model.fss.faces
        , amplitude = ( Just amplitudeX, Just amplitudeY, Just amplitudeZ )
        , lightSpeed = model.fss.lightSpeed
        , vignette = model.vignette.opacity
        , customSize = Nothing
        }


timeShiftRange : Float
timeShiftRange = 500.0


adaptTimeShift : String -> Time
adaptTimeShift v =
    let floatV = String.toFloat v
         |> Result.withDefault 0.0
    in (floatV - 50.0) / 100.0 * timeShiftRange


extractTimeShift : Time -> String
extractTimeShift v =
    (v / timeShiftRange * 100.0) + 50.0 |> toString


updateLayerDef
    :  Int
    -> (LayerDef -> LayerDef)
    -> Model
    -> Model
updateLayerDef index f model =
    let
        layersArray = Array.fromList model.layers
    in
        case layersArray |> Array.get index of
            Just layerDef ->
                { model
                | layers = layersArray
                    |> Array.set index (f layerDef)
                    |> Array.toList
                }
            Nothing -> model


updateLayer
    :  Int
    -> (WebGLLayer -> ModelChange -> WebGLLayer)
    -> (SVGLayer -> ModelChange -> SVGLayer)
    -> Model
    -> Model
updateLayer index ifWebgl ifSvg model =
    model |> updateLayerDef index
        (\layerDef ->
            { layerDef
            | layer = case layerDef.layer of
                Either.Left ( webglLayer, blend ) ->
                    Either.Left ( ifWebgl webglLayer layerDef.change, blend)
                Either.Right ( svgLayer, blend ) ->
                    Either.Right ( ifSvg svgLayer layerDef.change, blend)
            })


updateLayerBlend
    : Int
    -> (() -> Maybe WGLBlend.Blend)
    -> (() -> Maybe SVGBlend.Blend)
    -> Model
    -> Model
updateLayerBlend index ifWebgl ifSvg model =
    model |> updateLayerDef index
        (\layerDef ->
            { layerDef
            | layer = case layerDef.layer of
                Either.Left ( webglLayer, webglBlend ) ->
                    Either.Left
                        ( webglLayer
                        , ifWebgl () |> Maybe.withDefault webglBlend
                        )
                Either.Right ( svgLayer, svgBlend ) ->
                    Either.Right
                        ( svgLayer
                        , ifSvg () |> Maybe.withDefault svgBlend
                        )
            })


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ bang (\_ -> Bang)
        , AnimationFrame.diffs Animate
        , Window.resizes Resize
        -- , clicks (\pos ->
        --     toLocal model.size pos
        --         |> Maybe.map (\pos -> Pause)
        --         |> Maybe.withDefault NoOp
        --   )
        , moves (\pos ->
            toLocal model.size pos
                |> Maybe.map (\localPos -> Locate localPos)
                |> Maybe.withDefault NoOp
          )
        , rotate Rotate
        , changeProduct (\productStr -> Product.decode productStr |> ChangeProduct)
        , changeFacesX ChangeFacesX
        , changeFacesY ChangeFacesY
        , changeLightSpeed ChangeLightSpeed
        , changeAmplitude ChangeAmplitude
        , setCustomSize
            (\(w, h) ->
                let
                    (newW, newH) =
                        if (w > 0 && h > 0) then (w, h)
                        else model.size
                in
                    Window.Size newW newH |> Resize)
        , changeWGLBlend (\{ layer, blend } ->
            ChangeWGLBlend layer blend
          )
        , changeSVGBlend (\{ layer, blend } ->
            ChangeSVGBlend layer (SVGBlend.decode blend)
          )
        , configureLorenz (\(lorenzModel, layerIndex) ->
            Configure layerIndex (LorenzModel lorenzModel)
          )
        , configureFss (\(fssModel, layerIndex) ->
            FSS.fromPortModel fssModel |> FssModel |> Configure layerIndex
          )
        , rebuildFss (\(serializedMesh, layerIndex) ->
            RebuildFss layerIndex serializedMesh
          )
        , import_ Import
        , pause (\_ -> Pause)
        , continue (\_ -> Continue)
        , triggerPause (\_ -> TriggerPause)
        , turnOn TurnOn
        , turnOff TurnOff
        ]


adaptSize : Size -> Size
adaptSize (width, height) =
    ( toFloat width  * sizeCoef |> floor
    , toFloat height * sizeCoef |> floor
    )


getOrigin : Size -> Pos
getOrigin (width, height) =
    ( toFloat width  * (1 - sizeCoef) / 2 |> ceiling
    , toFloat height * (1 - sizeCoef) / 2 |> ceiling
    )


toLocal : (Int, Int) -> Position -> Maybe Position
toLocal (width, height) pos =
        if (pos.x <= width) && (pos.y <= height)
        then Just pos else Nothing



mapControls : Model -> Controls.Msg -> Msg
mapControls model controlsMsg =
    case controlsMsg of
        Controls.Configure cfg -> Configure 0 (LorenzModel cfg)
        Controls.Rotate th -> Rotate th


applyFssChange : ModelChange -> FSS.Model -> FSS.Model
applyFssChange change fssModel =
    case change <| FssModel fssModel of
        FssModel newModel -> newModel
        _ -> fssModel


changeIfFss : (FSS.Model -> FSS.Model) -> ModelChange
changeIfFss adjustFss =
    \model ->
        case model of
            FssModel prevModel -> adjustFss prevModel |> FssModel
            _ -> model


-- findTopAndGet : (Layer -> Int -> Maybe a) -> a -> Model -> a
-- findTopAndGet getF default model =
--     model.layers
--         |> List.indexedMap (,)
--         |> List.foldr (\(index, { layer }) result ->
--                 case result of
--                     Just result -> Just result
--                     Nothing -> getF layer index
--             ) Nothing
--         |> Maybe.withDefault default


isWebGLLayer : Layer -> Bool
isWebGLLayer layer =
    case layer of
        Either.Left _ -> True
        Either.Right _ -> False

isSvgLayer : Layer -> Bool
isSvgLayer layer =
    case layer of
        Either.Left _ -> False
        Either.Right _ -> True


mergeWebGLLayers : Model -> List WebGL.Entity
mergeWebGLLayers model =
    let viewport = getViewportState model |> Viewport.find
    in
        model.layers
            |> List.filter (.layer >> isWebGLLayer)
            |> List.filter .on
            |> List.indexedMap (,)
            -- |> List.concatMap (uncurry >> layerToEntities model viewport)
            |> List.concatMap (\(index, layer) ->
                    layerToEntities model viewport index layer
               )


mergeHtmlLayers : Model -> List (Html Msg)
mergeHtmlLayers model =
    model.layers
        |> List.filter (.layer >> isSvgLayer)
        |> List.filter .on
        |> List.indexedMap (layerToHtml model)


layerToHtml : Model -> Int -> LayerDef -> Html Msg
layerToHtml model index { layer } =
    case layer of
        Either.Right ( svgLayer, svgBlend ) ->
            case svgLayer of
                TextLayer ->
                    JbText.view model.product model.size model.origin svgBlend
                SvgImageLayer ->
                    SVGImage.view model.size model.origin model.product svgBlend
        _ -> div [] []


layerToEntities : Model -> Viewport {} -> Int -> LayerDef -> List WebGL.Entity
layerToEntities ({ fss } as model) viewport index ({ layer, change } as layerDef) =
    case layer of
        Either.Left ( webglLayer, blend ) ->
            case  webglLayer of
                LorenzLayer mesh ->
                    [ Lorenz.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                FractalLayer mesh ->
                    [ Fractal.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                TemplateLayer mesh ->
                    [ Template.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                VoronoiLayer mesh  ->
                    [ Voronoi.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                FssLayer serialized mesh ->
                    let
                        fssModel = applyFssChange change model.fss
                        ( maybeBorrowedSerialized, maybeBorrowedMesh ) =
                            ( serialized, mesh )
                            -- if (fssModel.shareMesh) then
                            --     model |>
                            --         findTopAndGet
                            --             (\layer otherIndex ->
                            --                 if otherIndex < index then
                            --                     case layer of
                            --                         FssLayer _ otherSerialized otherMesh ->
                            --                             Just ( otherSerialized, otherMesh )
                            --                         MirroredFssLayer _ otherSerialized otherMesh ->
                            --                             Just ( otherSerialized, otherMesh )
                            --                         _ -> Nothing
                            --                 else Nothing
                            --             )
                            --             ( serialized, mesh )
                            -- else ( serialized, mesh )
                    in
                        [ FSS.makeEntity
                            model.now
                            model.mouse
                            index
                            viewport
                            fssModel
                            maybeBorrowedSerialized
                            [ DepthTest.default, WGLBlend.produce blend, sampleAlphaToCoverage ]
                            maybeBorrowedMesh -- seems this mesh is already built with "Triangles", so there's no sense in reusing it
                    ]
                MirroredFssLayer serialized mesh ->
                    let
                        -- TODO: store clip position in the layer
                        model1 =
                            { model
                            | fss =
                                model.fss
                                    --|> applyFssChange changeF
                                    |> (\fss ->
                                        { fss
                                        | clip = Just (0.0, FSS.defaultMirror)
                                        , mirror = True
                                        }
                                    )
                            }
                        model2 =
                            { model
                            | fss =
                                model.fss
                                    -- |> applyFssChange changeF
                                    |> (\fss ->
                                        { fss
                                        | clip = Just ( FSS.defaultMirror, 1.0 )
                                        })
                            }
                    in
                        layerToEntities model1 viewport index
                            { layerDef
                            | layer = Either.Left (FssLayer serialized mesh, blend)
                            } ++
                        layerToEntities model2 viewport index
                            { layerDef
                            | layer = Either.Left (FssLayer serialized mesh, blend)
                            }
                VignetteLayer ->
                    [ Vignette.makeEntity
                        viewport
                        model.vignette
                        [ DepthTest.default, WGLBlend.produce blend, sampleAlphaToCoverage ]
                    ]
        _ -> []


getViewportState : Model -> Viewport.State
getViewportState { paused, size, origin, theta } =
    { paused = paused, size = size, origin = origin, theta = theta }


view : Model -> Html Msg
view model =
    div [ ]
        --span [ class "fps" ] [ toString model.fps ++ "FPS" |> text ]
        --    :: Html.map mapControls
        --     (config |>
        --           Controls.controls numVertices theta)
           --:: WebGL.toHtmlWith
        [ div
            [ H.class "overlay-panel import-export-panel hide-on-space" ]
            [ input
                [ type_ "range"
                , H.min "0"
                , H.max "100"
                , extractTimeShift model.timeShift |> H.value
                , onInput (\v -> adaptTimeShift v |> TimeTravel)
                , onMouseUp BackToNow
                ]
                []
            , input [ type_ "button", id "import-button", value "Import" ] [ text "Import" ]
            , input [ type_ "button", onClick Export, value "Export" ] [ text "Export" ]
            , input
                [ type_ "button", onClick ExportZip, value "Export .zip" ]
                [ text "Export .zip" ]
            ]
        , mergeWebGLLayers model |>
            WebGL.toHtmlWith
                [ WebGL.antialias
                , WebGL.alpha True
                , WebGL.clearColor 0.0 0.0 0.0 1.0
                --, WebGL.depth 0.5
                ]
                [ H.class "webgl-layers"
                , width (Tuple.first model.size)
                , height (Tuple.second model.size)
                , style
                    [ ( "display", "block" )
                    --, ( "background-color", "#161616" )
                    ,   ( "transform", "translate("
                        ++ (Tuple.first model.origin |> toString)
                        ++ "px, "
                        ++ (Tuple.second model.origin |> toString)
                        ++ "px)"
                        )
                    ]
                , onClick TriggerPause
                ]
        , mergeHtmlLayers model |> div [ H.class "svg-layers"]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


-- INCOMING PORTS

port bang : (() -> msg) -> Sub msg

port pause : (() -> msg) -> Sub msg

port continue : (() -> msg) -> Sub msg

port triggerPause : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port initLayers : (Array String -> msg) -> Sub msg

port configureLorenz : ((Lorenz.Model, Int) -> msg) -> Sub msg

port configureFss : ((FSS.PortModel, Int) -> msg) -> Sub msg

port configureMirroredFss : ((FSS.PortModel, Int) -> msg) -> Sub msg

port changeProduct : (String -> msg) -> Sub msg

port rebuildFss : ((FSS.SerializedScene, Int) -> msg) -> Sub msg

port turnOn : (Int -> msg) -> Sub msg

port turnOff : (Int -> msg) -> Sub msg

port import_ : (String -> msg) -> Sub msg

port changeFacesX : (Int -> msg) -> Sub msg

port changeFacesY : (Int -> msg) -> Sub msg

port changeLightSpeed : (Int -> msg) -> Sub msg

port changeVignette : (Float -> msg) -> Sub msg

port changeAmplitude : (FSS.AmplitudeChange -> msg) -> Sub msg

port setCustomSize : ((Int, Int) -> msg) -> Sub msg

port changeWGLBlend :
    ( { layer : Int
      , blend : WGLBlend.Blend
      }
    -> msg) -> Sub msg

port changeSVGBlend :
    ( { layer : Int
      , blend : SVGBlend.PortBlend
      }
    -> msg) -> Sub msg


-- OUTGOING PORTS

port startGui : GuiConfig -> Cmd msg

port requestFssRebuild : FssBuildOptions -> Cmd msg

port export_ : String -> Cmd msg

port exportZip_ : String -> Cmd msg

-- port rebuildOnClient : (FSS.SerializedScene, Int) -> Cmd msg

