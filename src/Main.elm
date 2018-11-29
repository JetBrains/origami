port module Main exposing (main)

import Array exposing (Array)
import AnimationFrame
import Time exposing (Time)
import Window
import Mouse exposing (clicks, moves, Position)
import Task exposing (Task)

import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
    exposing (class, width, height, style, class, type_, min, max, value, id)
import Html.Events exposing (on, onInput, onMouseUp, onClick)

import WebGL exposing (Mesh, Option)
import WebGL.Settings.Blend as B
import WebGL.Settings exposing (sampleAlphaToCoverage)
import WebGL.Settings.DepthTest as DepthTest

import Model exposing (..)
import Gui.Gui as Gui
import Viewport exposing (Viewport)
import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend
import Controls
import ImportExport as IE
import Product exposing (Product)
import Product as Product

import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.FSS as FSS
import Layer.Template as Template
import Layer.Cover as Cover
import Layer.Vignette as Vignette


sizeCoef : Float
sizeCoef = 1.0


initialMode : UiMode
initialMode = Production


init : ( Model, Cmd Msg )
init =
    ( Model.init initialMode initialLayers createLayer
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )


initialLayers : List ( LayerKind, String, LayerModel )
initialLayers =
    [ ( Fss, "Lower Layer", FssModel FSS.init )
    , ( Fss, "Mid Layer", FssModel FSS.init )
    , ( Fss, "Top layer"
      , let
            fssModel = FSS.init
        in
            { fssModel
            | renderMode = FSS.PartialLines
            , shareMesh = True
            } |> FssModel
      )
    , ( Cover, "Cover", NoModel )
    -- , ( Vignette, Vignette.init )
    ]
    |> List.filter (\(kind, _, _) ->
        case ( kind, initialMode ) of
            ( Cover, Ads ) -> False
            _ -> True
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Bang ->
            ( model
            , model |> IE.encodePortModel |> startGui
            )

        GuiMessage guiMsg ->
            (
                { model
                | gui = model.gui |> Gui.update guiMsg
                }
            , Cmd.none
            )

        Animate dt ->
            (
                { model
                 | fps = floor (1000 / dt)
                 , theta = if not (model.autoRotate || model.paused)
                              then model.theta + (dt * model.omega) / 1000
                              else model.theta
                 , now = if not model.paused
                            then model.now + dt + model.timeShift
                            else model.now
                 }
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

        HideControls ->
            ( { model | controlsVisible = False }
            , Cmd.none
            )

        Import encodedModel ->
            encodedModel
                |> IE.decodeModel initialMode createLayer
                |> Maybe.withDefault model
                |> rebuildAllFssLayersWith

        Export ->
            ( model
            , model |> IE.encodeModel |> export_
            )

        ExportZip ->
            ( model
            , model |> IE.encodeModel |> exportZip_
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

        Rotate omega ->
            ( { model | omega = omega  }
            , Cmd.none
            )

        Resize { width, height } ->
            ( { model
              | size = adaptSize ( width, height )
              , origin = getOrigin ( width, height )
              }
            , Cmd.none -- updateAndRebuildFssWith
            )

        ResizeFromPreset { width, height } ->
            let
                newModel =
                    { model
                    | size = adaptSize ( width, height )
                    , origin = getOrigin ( width, height )
                    }
            in
                ( newModel
                , newModel |> getSizeUpdate |> presetSizeChanged
                )

        Locate pos ->
            ( { model | mouse = (pos.x, pos.y) }
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

        MirrorOn index ->
            ( model |> updateLayerDef index
                (\layerDef ->
                    case layerDef.layer of
                        WebGLLayer webglLayer blend ->
                            case webglLayer of
                                FssLayer maybeScene mesh ->
                                    { layerDef
                                    | layer =
                                        WebGLLayer
                                        (MirroredFssLayer maybeScene mesh)
                                        blend
                                    , kind = MirroredFss
                                    }
                                _ -> layerDef
                        _ -> layerDef
                )
            , Cmd.none
            )

        MirrorOff index ->
            ( model |> updateLayerDef index
                (\layerDef ->
                    case layerDef.layer of
                        WebGLLayer webglLayer blend ->
                            case webglLayer of
                                MirroredFssLayer maybeScene mesh ->
                                    { layerDef
                                    | layer =
                                        WebGLLayer
                                        (FssLayer maybeScene mesh)
                                        blend
                                    , kind = Fss
                                    }
                                _ -> layerDef
                        _ -> layerDef
                )
            , Cmd.none
            )

        ChangeProduct product ->
            { model | product = product }
            |> rebuildAllFssLayersWith

        Configure index layerModel ->
            ( model |> updateLayer index
                (\layer layerModel ->
                    case layer of
                        WebGLLayer webglLayer webglBlend ->
                            WebGLLayer
                            (case ( webglLayer, layerModel ) of
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
                                ( VignetteLayer, VignetteModel vignetteModel ) ->
                                    VignetteLayer
                                _ -> webglLayer)
                            webglBlend
                        _ -> layer)
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

        ChangeFssRenderMode index renderMode ->
            -- ( model
            --     |> updateFss index
            --         (\fssModel -> { fssModel | renderMode = renderMode })
            -- , Cmd.none
            -- )
            model
                |> updateAndRebuildFssWith index
                    (\fssModel -> { fssModel | renderMode = renderMode })

        ChangeFaces index faces ->
            model
                |> updateAndRebuildFssWith index
                    (\fssModel -> { fssModel | faces = faces })

        ChangeLightSpeed index lightSpeed ->
            model
                |> updateAndRebuildFssWith index
                    (\fssModel -> { fssModel | lightSpeed = lightSpeed })

        RebuildFss index serializedScene ->
            ( model |> updateLayer index
                (\layer layerModel ->
                    case layer of
                        WebGLLayer webglLayer webglBlend ->
                            case ( webglLayer, layerModel ) of
                                ( FssLayer _ mesh, FssModel fssModel ) ->
                                    let
                                        maybeScene = Just serializedScene
                                        newMesh = maybeScene |> FSS.build fssModel
                                    in
                                        WebGLLayer
                                        (FssLayer maybeScene newMesh)
                                        webglBlend
                                ( MirroredFssLayer _ mesh, FssModel fssModel ) ->
                                    let
                                        maybeScene = Just serializedScene
                                        newMesh = maybeScene |> FSS.build fssModel
                                    in
                                        WebGLLayer
                                        (MirroredFssLayer maybeScene newMesh)
                                        webglBlend
                                _ -> layer
                        _ -> layer
                )
            , Cmd.none
            )

        ChangeVignette index opacity ->
            ( model
                |> updateFss index
                    (\fssModel -> { fssModel | vignette = opacity })
            , Cmd.none
            )

        ChangeIris index iris ->
            ( model |> updateFss index
                (\fssModel -> { fssModel | iris = iris })
            , Cmd.none
            )

            -- model
            --     |> updateAndRebuildFssWith index
            --         (\fssModel -> { fssModel | vignette = vignette })

            -- ( model
            --     |> updateLayerWithItsModel
            --         index
            --         (\(layer, model) ->
            --             case ( layer, model ) of
            --                 ( WebGLLayer VignetteLayer _, VignetteModel vignetteModel ) ->
            --                     (layer, { vignetteModel | opacity = opacity } |> VignetteModel)
            --                 _ -> (layer, model)
            --         )
            -- , Cmd.none

        ChangeAmplitude index ( newAmplitudeX, newAmplitudeY, newAmplitudeZ ) ->
            model
                |> updateAndRebuildFssWith index
                    (\fss ->
                        let
                            ( currentAmplitudeX, currentAmplitudeY, currentAmplitudeZ )
                                = fss.amplitude
                        in
                            { fss | amplitude =
                                ( Maybe.withDefault currentAmplitudeX newAmplitudeX
                                , Maybe.withDefault currentAmplitudeY newAmplitudeY
                                , Maybe.withDefault currentAmplitudeZ newAmplitudeZ
                                )
                            }
                    )


        ShiftColor index ( newHue, newSaturation, newBrightness ) ->
            ( model |> updateFss index
                (\fss ->
                    let
                        ( currentHue, currentSaturation, currentBrightness )
                            = fss.colorShift
                    in
                        { fss | colorShift =
                            ( Maybe.withDefault currentHue newHue
                            , Maybe.withDefault currentSaturation newSaturation
                            , Maybe.withDefault currentBrightness newBrightness
                            )
                        }
                )
            , Cmd.none
            )

        SavePng ->
            ( model
            , model |> getSizeUpdate |> triggerSavePng
            )

        ApplyRandomizer portModel ->
            IE.decodePortModel createLayer portModel
                |> rebuildAllFssLayersWith

        NoOp -> ( model, Cmd.none )


getSizeUpdate : Model -> SizeUpdate
getSizeUpdate model =
    { size = model.size
    , product = Product.encode model.product
    , coverSize = Product.getCoverTextSize model.product
    }


getLayerModel : LayerIndex -> Model -> Maybe LayerModel
getLayerModel index model =
    model.layers
        |> Array.fromList
        |> Array.get index
        |> Maybe.map .model


updateFss : LayerIndex -> (FSS.Model -> FSS.Model) -> Model -> Model
updateFss index f model =
    model |> updateLayerWithItsModel
        index
        (\(layer, layerModel) ->
            ( layer
            , case layerModel of
                FssModel fssModel ->
                    f fssModel |> FssModel
                _ -> layerModel
            )
        )


updateAndRebuildFssWith
    : LayerIndex
    -> (FSS.Model -> FSS.Model)
    -> Model
    -> ( Model, Cmd Msg )
updateAndRebuildFssWith index f curModel =
    let
        newModel = updateFss index f curModel
    in
        ( newModel
        , case (newModel |> getLayerModel index) of
            Just (FssModel fssModel) ->
                requestFssRebuild
                    { layer = index
                    , model = IE.encodePortModel newModel
                    , value = IE.encodeFss fssModel newModel.product
                    }
            _ -> Cmd.none
        )


rebuildAllFssLayersWith : Model -> ( Model, Cmd Msg )
rebuildAllFssLayersWith model =
    let
        isLayerFss layerDef =
            case layerDef.model of
                FssModel fssModel -> Just fssModel
                _ -> Nothing
        encodedModel = IE.encodePortModel model
        rebuildPotentialFss index fssModel =
            requestFssRebuild
                { layer = index
                , model = encodedModel
                , value = IE.encodeFss fssModel model.product
                }

    in
        ( model
        , List.filterMap isLayerFss model.layers
          |> List.indexedMap rebuildPotentialFss
          |> Cmd.batch
        )


getBlendForPort : Layer -> PortBlend
getBlendForPort layer =
    ( case layer of
        WebGLLayer _ webglBlend -> Just webglBlend
        _ -> Nothing
    , case layer of
        SVGLayer _ svgBlend ->
            SVGBlend.encode svgBlend |> Just
        _ -> Nothing
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
        Cover -> "cover"
        Vignette -> "vignette"
        Empty -> "empty"


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


createLayer : LayerKind -> LayerModel -> Layer
createLayer kind layerModel =
    case ( kind, layerModel ) of
        ( Fss, FssModel fssModel )  ->
            WebGLLayer
            ( FSS.build fssModel Nothing |> FssLayer Nothing )
            WGLBlend.default
        ( MirroredFss, FssModel fssModel ) ->
            WebGLLayer
            ( FSS.build fssModel Nothing |> MirroredFssLayer Nothing )
            WGLBlend.default
            -- (WGLBlend.build
            --    (B.customAdd, B.oneMinusSrcColor, B.oneMinusSrcColor)
            --    (B.customAdd, B.srcColor, B.zero)
            -- )
        ( Lorenz, LorenzModel lorenzModel ) ->
            WebGLLayer
            (Lorenz.build lorenzModel |> LorenzLayer)
            WGLBlend.default
        ( Template, TemplateModel templateModel ) ->
            WebGLLayer
            ( Template.build templateModel |> TemplateLayer )
            WGLBlend.default
        ( Voronoi, VoronoiModel voronoiModel ) ->
            WebGLLayer
            ( Voronoi.build voronoiModel |> VoronoiLayer )
            WGLBlend.default
        ( Fractal, FractalModel fractalModel ) ->
            WebGLLayer
            ( Fractal.build fractalModel |> FractalLayer )
            WGLBlend.default
        ( Vignette, _ ) ->
            WebGLLayer
            VignetteLayer
            (WGLBlend.build
                (B.customAdd, B.srcAlpha, B.oneMinusSrcAlpha)
                (B.customAdd, B.one, B.oneMinusSrcAlpha) )
            -- WGLBlend.Blend Nothing (0, 1, 7) (0, 1, 7) |> VignetteLayer Vignette.init
            -- VignetteLayer Vignette.init WGLBlend.default
        ( Cover, _ ) ->
            SVGLayer
            CoverLayer
            SVGBlend.default
        _ ->
            Model.emptyLayer


-- extractFssBuildOptions : Model -> FssBuildOptions
-- extractFssBuildOptions = prepareGuiConfig



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
    -> (Layer -> LayerModel -> Layer)
    -> Model
    -> Model
updateLayer index f model =
    model |> updateLayerDef index
        (\layerDef ->
            { layerDef
            | layer = f layerDef.layer layerDef.model
            })


updateLayerWithItsModel
    :  Int
    -> (( Layer, LayerModel ) -> ( Layer, LayerModel ))
    -> Model
    -> Model
updateLayerWithItsModel index f model =
    model |> updateLayerDef index
        (\layerDef ->
            case f (layerDef.layer, layerDef.model) of
                ( newLayer, newModel ) ->
                    { layerDef
                    | layer = newLayer
                    , model = newModel
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
                WebGLLayer webglLayer webglBlend ->
                    ifWebgl ()
                        |> Maybe.withDefault webglBlend
                        |> WebGLLayer webglLayer
                SVGLayer svgLayer svgBlend ->
                    ifSvg ()
                        |> Maybe.withDefault svgBlend
                        |> SVGLayer svgLayer
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
        , changeFssRenderMode (\{value, layer} ->
            IE.decodeFssRenderMode value |> ChangeFssRenderMode layer)
        , changeFacesX (\{value, layer} ->
            case model |> getLayerModel layer of
                Just (FssModel { faces }) ->
                    case faces of
                        ( _, facesY ) -> ChangeFaces layer ( value, facesY )
                _ -> NoOp
          )
        , changeFacesY (\{value, layer} ->
            case model |> getLayerModel layer of
                Just (FssModel { faces }) ->
                    case faces of
                        ( facesX, _ ) -> ChangeFaces layer ( facesX, value )
                _ -> NoOp
          )
        , changeLightSpeed (\{value, layer} -> ChangeLightSpeed layer value)
        , changeAmplitude (\{value, layer} -> ChangeAmplitude layer value)
        , shiftColor (\{value, layer} -> ShiftColor layer value)
        , changeVignette (\{value, layer} -> ChangeVignette layer value)
        , changeIris (\{value, layer} -> ChangeIris layer value)
        , setCustomSize
            (\(w, h) ->
                let
                    (newW, newH) =
                        if (w > 0 && h > 0) then (w, h)
                        else model.size
                in
                    Window.Size newW newH |> ResizeFromPreset)
        , changeWGLBlend (\{ layer, value } ->
            ChangeWGLBlend layer value
          )
        , changeSVGBlend (\{ layer, value } ->
            ChangeSVGBlend layer (SVGBlend.decode value)
          )
        , configureLorenz (\{ layer, value } ->
            Configure layer (LorenzModel value)
          )
        , configureFss (\{ layer, value } ->
            IE.fromFssPortModel value |> FssModel |> Configure layer
          )
        , rebuildFss (\{ layer, value } ->
            RebuildFss layer value
          )
        , applyRandomizer ApplyRandomizer
        , import_ Import
        , pause (\_ -> Pause)
        , continue (\_ -> Continue)
        , triggerPause (\_ -> TriggerPause)
        , hideControls (\_ -> HideControls)
        , turnOn TurnOn
        , turnOff TurnOff
        , mirrorOn MirrorOn
        , mirrorOff MirrorOff
        , savePng (\_ -> SavePng)
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


isWebGLLayer : Layer -> Bool
isWebGLLayer layer =
    case layer of
        WebGLLayer _ _ -> True
        SVGLayer _ _ -> False

isSvgLayer : Layer -> Bool
isSvgLayer layer =
    case layer of
        WebGLLayer _ _ -> False
        SVGLayer _ _ -> True


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
        SVGLayer svgLayer svgBlend ->
            case svgLayer of
                CoverLayer ->
                    Cover.view initialMode model.product model.size model.origin svgBlend
                NoContent -> div [] []
        _ -> div [] []


layerToEntities : Model -> Viewport {} -> Int -> LayerDef -> List WebGL.Entity
layerToEntities model viewport index layerDef =
    case layerDef.layer of
        WebGLLayer webglLayer blend ->
            case ( webglLayer, layerDef.model ) of
                ( LorenzLayer mesh, _ ) ->
                    [ Lorenz.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( FractalLayer mesh, _ ) ->
                    [ Fractal.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( TemplateLayer mesh, _ ) ->
                    [ Template.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( VoronoiLayer mesh, _ ) ->
                    [ Voronoi.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( FssLayer serialized mesh, FssModel fssModel ) ->
                    let
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
                ( MirroredFssLayer serialized mesh, FssModel fssModel ) ->
                    let
                        -- TODO: store clip position in the layer
                        fssModel1 =
                            { fssModel
                            | clip = Just (0.0, FSS.defaultMirror)
                            , mirror = True
                            }
                        fssModel2 =
                            { fssModel
                            | clip = Just ( FSS.defaultMirror, 1.0 )
                            }
                    in
                        layerToEntities model viewport index
                            { layerDef
                            | layer = WebGLLayer (FssLayer serialized mesh) blend
                            , model = FssModel fssModel1
                            } ++
                        layerToEntities model viewport index
                            { layerDef
                            | layer = WebGLLayer (FssLayer serialized mesh) blend
                            , model = FssModel fssModel2
                            }
                ( VignetteLayer, _ ) ->
                    [ Vignette.makeEntity
                        viewport
                        Vignette.init
                        [ DepthTest.default, WGLBlend.produce blend, sampleAlphaToCoverage ]
                    ]
                _ -> []
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
        [ mergeHtmlLayers model |> div [ H.class "svg-layers" ]
        , if model.controlsVisible
            then ( div
                [ H.class "overlay-panel import-export-panel hide-on-space" ]
                [
                  div [  H.class "timeline_holder" ] [
                  span [ H.class "label past"] [text "past"]
                , input
                    [ type_ "range"
                    , class "timeline"
                    , H.min "0"
                    , H.max "100"
                    , extractTimeShift model.timeShift |> H.value
                    , onInput (\v -> adaptTimeShift v |> TimeTravel)
                    , onMouseUp BackToNow
                    ]
                    []
                , span [ H.class "label future"] [text "future"]
                  ]
                -- , input [ type_ "button", id "import-button", value "Import" ] [ text "Import" ]
                -- , input [ type_ "button", onClick Export, value "Export" ] [ text "Export" ]
                , input
                    [ type_ "button", class "export_html5", onClick ExportZip, value "warp in html5" ]
                    [ text "Export to html5.zip" ]
                , input
                    [ type_ "button", class "export_png", onClick SavePng, value "blast to png" ]
                    [ text "Export to png" ]
                , div [ H.class "spacebar_info" ] [ text "spacebar to hide controls, click to pause" ]
                ]
            ) else div [] []
        , mergeWebGLLayers model |>
            WebGL.toHtmlWith
                [ WebGL.antialias
                , WebGL.alpha True
                , WebGL.clearColor 0.0 0.0 0.0 1.0
                -- , WebGL.depth 0.5
                ]
                [ H.class "webgl-layers"
                , width (Tuple.first model.size)
                , height (Tuple.second model.size)
                , style
                    [ ( "display", "block" )
                    --, ( "background-color", "#161616" )
--                    ,   ( "transform", "translate("
--                        ++ (Tuple.first model.origin |> toString)
--                        ++ "px, "
--                        ++ (Tuple.second model.origin |> toString)
--                        ++ "px)"
--                        )
                    ]
                , onClick TriggerPause
                ]
        -- , mergeHtmlLayers model |> div [ H.class "svg-layers"]
        , Gui.view model.gui |> Html.map GuiMessage
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

port hideControls : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port initLayers : (Array String -> msg) -> Sub msg

port configureLorenz : ({ value: Lorenz.Model, layer: LayerIndex } -> msg) -> Sub msg

port configureFss : ({ value: FSS.PortModel, layer: LayerIndex } -> msg) -> Sub msg

port configureMirroredFss : ({ value: FSS.PortModel, layer: LayerIndex } -> msg) -> Sub msg

port changeProduct : (String -> msg) -> Sub msg

port rebuildFss : ({ value: FSS.SerializedScene, layer: LayerIndex } -> msg) -> Sub msg

port turnOn : (LayerIndex -> msg) -> Sub msg

port turnOff : (LayerIndex -> msg) -> Sub msg

port mirrorOn : (LayerIndex -> msg) -> Sub msg

port mirrorOff : (LayerIndex -> msg) -> Sub msg

port import_ : (String -> msg) -> Sub msg

port changeFssRenderMode : ({ value: String, layer: LayerIndex } -> msg) -> Sub msg

port changeFacesX : ({ value: Int, layer: LayerIndex } -> msg) -> Sub msg

port changeFacesY : ({ value: Int, layer: LayerIndex } -> msg) -> Sub msg

port changeLightSpeed : ({ value: Int, layer: LayerIndex } -> msg) -> Sub msg

port changeVignette : ({ value: FSS.Vignette, layer: LayerIndex } -> msg) -> Sub msg

port changeIris : ({ value: FSS.Iris, layer: LayerIndex } -> msg) -> Sub msg

port changeAmplitude : ({ value: FSS.AmplitudeChange, layer: LayerIndex } -> msg) -> Sub msg

port shiftColor : ({ value: FSS.ColorShiftPatch, layer: LayerIndex } -> msg) -> Sub msg

port setCustomSize : ((Int, Int) -> msg) -> Sub msg

port applyRandomizer : (PortModel -> msg) -> Sub msg

port savePng : (() -> msg) -> Sub msg

port changeWGLBlend :
    ( { layer : Int
      , value : WGLBlend.Blend
      }
    -> msg) -> Sub msg

port changeSVGBlend :
    ( { layer : Int
      , value : String
      }
    -> msg) -> Sub msg


-- OUTGOING PORTS

type alias SizeUpdate =
    { size: Size
    , product: String
    , coverSize: Size
    }

port startGui : PortModel -> Cmd msg

port requestFssRebuild :
    { layer: LayerIndex
    , model: PortModel
    , value: FSS.PortModel
    } -> Cmd msg

port presetSizeChanged : SizeUpdate -> Cmd msg

port export_ : String -> Cmd msg

port exportZip_ : String -> Cmd msg

port triggerSavePng : SizeUpdate -> Cmd msg

-- port rebuildOnClient : (FSS.SerializedScene, Int) -> Cmd msg
