port module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H exposing (width, height, style, class, type_, min, max, value, id)
import Html.Events exposing (on, onInput, onMouseUp, onClick)
import AnimationFrame
import Time exposing (Time)
import Window
import Mouse exposing (clicks, moves, Position)
import Task exposing (Task)
import WebGL exposing (Mesh, Option)
import WebGL.Settings exposing (sampleAlphaToCoverage)
import WebGL.Settings.DepthTest as DepthTest

import Viewport exposing (Viewport)
import Blend exposing (Blend)
import Controls
import ImportExport as IE exposing (EncodedState)

import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.FSS as FSS
import Layer.Template as Template



type alias LayerIndex = Int

type LayerConfig
    -- FIXME: use type variable for that, or just a function!
    = LorenzConfig Lorenz.Config
    | FractalConfig Fractal.Config
    | VoronoiConfig Voronoi.Config
    | FssConfig FSS.Config
    | MirroredFssConfig FSS.MConfig
    | TemplateConfig Template.Config


type Layer
    -- FIXME: use type variable for that, or just a function!
    = LorenzLayer Lorenz.Config Blend Lorenz.Mesh
    | FractalLayer Fractal.Config Blend Fractal.Mesh
    | VoronoiLayer Voronoi.Config Blend Voronoi.Mesh
    | TemplateLayer Template.Config Blend Template.Mesh
    | FssLayer FSS.Config Blend (Maybe FSS.SerializedScene) FSS.Mesh
    | MirroredFssLayer FSS.MConfig Blend (Maybe FSS.SerializedScene) FSS.Mesh
    | TextLayer Blend
    | Unknown
    -- | CanvasLayer (\_ -> )


type alias Model =
    { paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , layers : List Layer
    , size : ( Int, Int )
    , mouse : ( Int, Int)
    , now : Time
    , timeShift : Time
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | InitLayers (List String)
    | Configure LayerIndex LayerConfig
    | ChangeBlend LayerIndex Blend
    | RebuildFss LayerIndex FSS.SerializedScene
    | Rotate Float
    | Locate Position
    | Import EncodedState
    | Export
    | TimeTravel Float
    | BackToNow
    | Pause
    | Continue
    | TriggerPause
    | NoOp


emptyModel : Model
emptyModel =
    { paused = False
    , autoRotate = False
    , fps = 0
    , theta = 0.1
    , layers = [ ]
    , size = ( 1500, 800 )
    , mouse = ( 0, 0 )
    , now = 0.0
    , timeShift = 0.0
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

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

        InitLayers layerTypes ->
            ( { model | layers = layerTypes |> List.map createLayer }
            , Cmd.none
            )

        Configure index config ->
            ( model |> updateLayer index
                (\layer ->
                    case ( layer, config ) of
                        -- FIXME: simplify
                        ( LorenzLayer _ curBlend _, LorenzConfig lorenzConfig ) ->
                            LorenzLayer lorenzConfig curBlend (lorenzConfig |> Lorenz.build)
                        ( FractalLayer _ curBlend _, FractalConfig fractalConfig ) ->
                            FractalLayer fractalConfig curBlend (fractalConfig |> Fractal.build)
                        ( VoronoiLayer _ curBlend _, VoronoiConfig voronoiConfig ) ->
                            VoronoiLayer voronoiConfig curBlend (voronoiConfig |> Voronoi.build)
                        ( FssLayer _ curBlend maybeScene _, FssConfig fssConfig ) ->
                            let
                                newMesh = maybeScene |> FSS.build fssConfig
                            in
                                FssLayer fssConfig curBlend maybeScene newMesh
                        ( MirroredFssLayer _ curBlend maybeScene _
                        , MirroredFssConfig mirroredFssConfig ) ->
                            let
                                fssConfig = FSS.loadConfig mirroredFssConfig
                                newMesh = maybeScene
                                    |> FSS.build fssConfig
                            in
                                MirroredFssLayer mirroredFssConfig curBlend maybeScene newMesh
                        ( TemplateLayer _ curBlend _, TemplateConfig templateConfig ) ->
                            TemplateLayer templateConfig curBlend (templateConfig |> Template.build)
                        _ -> Unknown
                )
            , Cmd.none
            )

        ChangeBlend index newBlend ->
            ( model |> updateLayer index
                (\layer ->
                    case layer of
                        -- FIXME: simplify
                        TemplateLayer cfg _ mesh ->
                            TemplateLayer cfg newBlend mesh
                        LorenzLayer cfg _ mesh ->
                            LorenzLayer cfg newBlend mesh
                        FractalLayer cfg _ mesh ->
                            FractalLayer cfg newBlend mesh
                        VoronoiLayer cfg _ mesh ->
                            VoronoiLayer cfg newBlend mesh
                        FssLayer cfg _ scene mesh ->
                            FssLayer cfg newBlend scene mesh
                        MirroredFssLayer cfg _ scene mesh ->
                            MirroredFssLayer cfg newBlend scene mesh
                        TextLayer _ ->
                            TextLayer newBlend
                        _ -> Unknown
                )
            , Cmd.none
            )

        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )

        Resize { width, height } ->
            ( { model | size = ( width, height ) }
            , Cmd.none
            )

        Locate pos ->
            ( { model | mouse = (pos.x, pos.y) }
            , Cmd.none
            )

        RebuildFss index serializedScene ->
            ( model |> updateLayer index
                (\layer ->
                    case layer of
                        FssLayer cfg blend _ mesh ->
                            let
                                maybeScene = Just serializedScene
                                newMesh = maybeScene |> FSS.build cfg
                            in
                                FssLayer cfg blend maybeScene newMesh
                        MirroredFssLayer mirrorCfg blend _ mesh ->
                            let
                                maybeScene = Just serializedScene
                                newMesh = maybeScene |> FSS.build (FSS.loadConfig mirrorCfg)
                            in
                                MirroredFssLayer mirrorCfg blend maybeScene newMesh
                        _ -> layer
                )
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
                    , layers = List.map2 extractLayer model.layers src.layers
                    , mouse = src.mouse
                    , size = src.size
                    } )
                |> Debug.log "decoded model"
                |> Maybe.withDefault model
            , Cmd.none )

        Export ->
            ( model
            , model |> prepareModel |> IE.encodeModel |> export_
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

        _ -> ( model, Cmd.none )


createLayer : String -> Layer
createLayer code =
    case code of
        "fss" ->
            let fssConfig = FSS.init
            in
                FssLayer
                    fssConfig
                    Blend.default
                    Nothing
                    (FSS.build fssConfig Nothing)
        "fss-mirror" ->
            let mirrorConfig = FSS.initM
                fssConfig = FSS.init
            in
                MirroredFssLayer
                    mirrorConfig
                    Blend.default
                    Nothing
                    (FSS.build fssConfig Nothing)
        "lorenz" ->
            let lorenzConfig = Lorenz.init
            in LorenzLayer lorenzConfig Blend.default (lorenzConfig |> Lorenz.build)
        "template" ->
            let templateConfig = Template.init
            in TemplateLayer templateConfig Blend.default (templateConfig |> Template.build)
        "voronoi" ->
            let voronoiConfig = Voronoi.init
            in VoronoiLayer voronoiConfig Blend.default (voronoiConfig |> Voronoi.build)
        "fractal" ->
            let fractalConfig = Fractal.init
            in FractalLayer fractalConfig Blend.default (fractalConfig |> Fractal.build)
        -- TODO: text
        _ -> Unknown


extractLayer : Layer -> IE.Layer -> Layer
extractLayer curLayer srcLayer =
    case ( srcLayer.type_, curLayer ) of
        ( IE.MirroredFss,  MirroredFssLayer config blend scene mesh ) ->
            MirroredFssLayer config srcLayer.blend scene mesh
        _ -> Unknown


prepareLayer : Layer -> IE.Layer
prepareLayer layer =
    case layer of
        FssLayer config blend maybeScene mesh ->
            { type_ = IE.Fss
            , blend = blend
            -- , config = config
            -- , mesh = mesh
            }
        MirroredFssLayer config blend maybeScene mesh ->
            { type_ = IE.MirroredFss
            , blend = blend
            -- , config = config
            -- , mesh = mesh
            }
        _ -> IE.defaultLayer


prepareModel : Model -> IE.Model
prepareModel model =
    { theta = model.theta
    , now = model.now
    , layers = List.map prepareLayer model.layers
    , mouse = model.mouse
    , size = model.size
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


configureWhenMatches : LayerIndex -> Model -> LayerConfig -> (Layer -> Bool) -> Msg
configureWhenMatches index { layers } config f =
    case Array.fromList layers|>Array.get index of
        Just layer -> if (f layer) then Configure index config else NoOp
        Nothing -> NoOp


updateLayer : Int -> (Layer -> Layer) -> Model -> Model
updateLayer index f model =
    let layersArray = Array.fromList model.layers
    in
        case layersArray |> Array.get index of
            Just layer ->
                { model
                | layers = layersArray |> Array.set index (f layer) |> Array.toList
                }
            Nothing -> model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
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
        , initLayers (\layerTypes -> InitLayers (Array.toList layerTypes))
        , changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
          )
        , configureLorenz (\(lorenzConfig, layerIndex) ->
            configureWhenMatches layerIndex model (LorenzConfig lorenzConfig)
                (\layer ->
                    case layer of
                        LorenzLayer _ _ _ -> True
                        _ -> False
                )
          )
        , configureFss (\(fssConfig, layerIndex) ->
            configureWhenMatches layerIndex model (FssConfig fssConfig)
                (\layer ->
                    case layer of
                        FssLayer _ _ _ _ -> True
                        _ -> False
                )
          )
        , configureMirroredFss (\(mirroredFssConfig, layerIndex) ->
            configureWhenMatches layerIndex model (MirroredFssConfig mirroredFssConfig)
                (\layer ->
                    case layer of
                        MirroredFssLayer _ _ _ _ -> True
                        _ -> False
                )
          )
        , rebuildFss (\(serializedMesh, layerIndex) ->
            RebuildFss layerIndex serializedMesh
          )
        , import_ Import
        , pause (\_ -> Pause)
        , continue (\_ -> Continue)
        , triggerPause (\_ -> TriggerPause)
        ]


toLocal : (Int, Int) -> Position -> Maybe Position
toLocal (width, height) pos =
        if (pos.x <= width) && (pos.y <= height)
        then Just pos else Nothing



mapControls : Model -> Controls.Msg -> Msg
mapControls model controlsMsg =
    case controlsMsg of
        Controls.Configure cfg -> Configure 0 (LorenzConfig cfg)
        Controls.Rotate th -> Rotate th


mergeLayers : Model -> List WebGL.Entity
mergeLayers model =
    let viewport = getViewportState model |> Viewport.find
    in
        model.layers |> List.concatMap (layerToEntities model viewport)


layerToEntities : Model -> Viewport {} -> Layer -> List WebGL.Entity
layerToEntities model viewport layer =
    case layer of
        LorenzLayer _ blend lorenz ->
            [ Lorenz.makeEntity
                viewport
                [ DepthTest.default, Blend.produce blend ]
                lorenz
            ]
        FractalLayer _ blend fractal ->
            [ Fractal.makeEntity
                viewport
                [ DepthTest.default, Blend.produce blend ]
                fractal
            ]
        TemplateLayer _ blend template ->
            [ Template.makeEntity
                viewport
                [ DepthTest.default, Blend.produce blend ]
                template
            ]
        VoronoiLayer _ blend voronoi ->
            [ Voronoi.makeEntity
                viewport
                [ DepthTest.default, Blend.produce blend ]
                voronoi
            ]
        FssLayer config blend serialized fss ->
            [ FSS.makeEntity
                viewport
                model.now
                model.mouse
                config
                serialized
                [ DepthTest.default, Blend.produce blend, sampleAlphaToCoverage ]
                fss
            ]
        MirroredFssLayer mConfig blend serialized fss ->
            let
                fssConfig = FSS.loadConfig mConfig
                config1 = { fssConfig | clip = ( mConfig.mirror, 1 ) }
                config2 = { fssConfig | clip = ( 0, mConfig.mirror ), hasMirror = True }
            in
                (layerToEntities model viewport (FssLayer config1 blend serialized fss)) ++
                (layerToEntities model viewport (FssLayer config2 blend serialized fss))
        TextLayer blend ->
            -- FIXME: replace with text
            [ Template.makeEntity
                viewport
                [ DepthTest.default, Blend.produce blend ]
                (Template.init |> Template.build)
            ]
        Unknown ->
            [ Template.makeEntity
                viewport
                [ DepthTest.default, Blend.produce Blend.default ]
                (Template.init |> Template.build)
            ]


getViewportState : Model -> Viewport.State
getViewportState { paused, size, theta } =
    { paused = paused, size = size, theta = theta }


view : Model -> Html Msg
view model =
    div [ ]
        --span [ class "fps" ] [ toString model.fps ++ "FPS" |> text ]
        --    :: Html.map mapControls
        --     (config |>
        --           Controls.controls numVertices theta)
           --:: WebGL.toHtmlWith
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
          , WebGL.toHtmlWith
              [ WebGL.antialias
              , WebGL.alpha True
              , WebGL.clearColor 0.0862745098 0.0862745098 0.0862745098 1.0
              --, WebGL.depth 0.5
              ]
              [ width (Tuple.first model.size)
              , height (Tuple.second model.size)
              , style [ ( "display", "block" ), ("background-color", "#161616") ]
              , onClick TriggerPause
              ]
              (mergeLayers model)
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port pause : (() -> msg) -> Sub msg

port continue : (() -> msg) -> Sub msg

port triggerPause : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port initLayers : (Array String -> msg) -> Sub msg

port configureLorenz : ((Lorenz.Config, Int) -> msg) -> Sub msg

port configureFss : ((FSS.Config, Int) -> msg) -> Sub msg

port configureMirroredFss : ((FSS.MConfig, Int) -> msg) -> Sub msg

-- TODO: port to affect camera

port rebuildFss : ((FSS.SerializedScene, Int) -> msg) -> Sub msg

port import_ : (String -> msg) -> Sub msg

port export_ : String -> Cmd msg

port changeBlend :
    ( { layer : Int
      , blend : Blend
      }
    -> msg) -> Sub msg
