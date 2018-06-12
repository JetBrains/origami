port module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html, text, div, span)
import Html.Attributes exposing (width, height, style, class)
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
    | TemplateConfig Template.Config


type Layer
    -- FIXME: use type variable for that, or just a function!
    = LorenzLayer Lorenz.Config Blend Lorenz.Mesh
    | FractalLayer Fractal.Config Blend Fractal.Mesh
    | VoronoiLayer Voronoi.Config Blend Voronoi.Mesh
    | TemplateLayer Template.Config Blend Template.Mesh
    | FssLayer FSS.Config Blend (Maybe FSS.SerializedScene) FSS.Mesh
    | MirroredFssLayer FSS.Config MirrorConfig Blend (Maybe FSS.SerializedScene) FSS.Mesh
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
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | Configure LayerIndex LayerConfig
    | ChangeBlend LayerIndex Blend
    | RebuildFss FSS.SerializedScene
    | Rotate Float
    | Locate Position
    | Pause
    | Start
    | NoOp


type alias MirrorConfig =
    { mirrorPos : Float
    }


init : ( Model, Cmd Msg )
init =
    let
        -- lorenzConfig = Lorenz.init
        -- fractalConfig = Fractal.init
        -- voronoiConfig = Voronoi.init
        -- templateConfig = Template.init
        fssConfig = FSS.init
    in
        (
            { paused = False
            , autoRotate = False
            , fps = 0
            , theta = 0.1
            , layers =
                [ MirroredFssLayer
                    fssConfig
                    { mirrorPos = 0.5 }
                    Blend.default
                    Nothing
                    (FSS.build fssConfig Nothing)
                ]
                -- [ TemplateLayer templateConfig Blend.default (templateConfig |> Template.build)
                -- , FssLayer fssConfig Blend.default Nothing (FSS.build fssConfig Nothing)
                -- , LorenzLayer lorenzConfig Blend.default (lorenzConfig |> Lorenz.build)
                -- , FractalLayer fractalConfig Blend.default (fractalConfig |> Fractal.build)
                -- , VoronoiLayer voronoiConfig Blend.default (voronoiConfig |> Voronoi.build)
                -- ]
            , size = ( 1500, 800 )
            , now = 0.0
            , mouse = ( 0, 0 )
            }
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
                 , theta = if not model.autoRotate then model.theta + dt / 4000 else model.theta
                 , now = if not model.paused then model.now + dt else model.now
                 }
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
                                newMesh = (maybeScene |> FSS.build fssConfig)
                            in
                                FssLayer fssConfig curBlend maybeScene newMesh
                        ( MirroredFssLayer _ mirror curBlend maybeScene _, FssConfig fssConfig ) ->
                            let
                                newMesh = (maybeScene |> FSS.build fssConfig)
                            in
                                MirroredFssLayer fssConfig mirror curBlend maybeScene newMesh
                        ( TemplateLayer _ curBlend _, TemplateConfig templateConfig ) ->
                            TemplateLayer templateConfig curBlend (templateConfig |> Template.build)
                        _ -> Unknown
                )
            , Cmd.none )

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
                        MirroredFssLayer cfg mirror _ scene mesh ->
                            MirroredFssLayer cfg mirror newBlend scene mesh
                        TextLayer _ ->
                            TextLayer newBlend
                        _ -> Unknown
                )
            , Cmd.none )

        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )

        Resize { width, height } ->
            ( model
            , Cmd.none
            )

        Locate pos ->
            (
                { model | mouse = (pos.x, pos.y)

                }
            , Cmd.none
            )

        RebuildFss serializedScene ->
            ( model
                |> changeAll
                    (\layer ->
                        case layer of
                            FssLayer cfg blend _ mesh ->
                                let
                                    maybeScene = Just serializedScene
                                    newMesh = maybeScene |> FSS.build cfg
                                in
                                    FssLayer cfg blend maybeScene newMesh |> Just
                            MirroredFssLayer cfg mirror blend _ mesh ->
                                let
                                    maybeScene = Just serializedScene
                                    newMesh = maybeScene |> FSS.build cfg
                                in
                                    MirroredFssLayer cfg mirror blend maybeScene newMesh |> Just
                            _ -> Nothing
                    )
            , Cmd.none
            )

        Pause ->
          ( { model | paused = not model.paused }
          , Cmd.none
          )

        _ -> ( model, Cmd.none )


changeAll : (Layer -> Maybe Layer) -> Model -> Model
changeAll f model =
    { model |
        layers =
            model.layers |>
                List.map (\layer ->
                    case f layer of
                        Just newLayer -> newLayer
                        Nothing -> layer
                )
    }


configureFirst : Model -> LayerConfig -> (Layer -> Bool) -> Msg
configureFirst { layers } config f =
    layers
        |> List.foldl
            (\layer (lastIdx, indices) ->
                ( lastIdx + 1
                , if f layer
                    then lastIdx :: indices
                    else indices
                )
            )
            (0, [])
        |> Tuple.second
        |> List.head
        |> Maybe.withDefault 0
        |> \idx -> Configure idx config


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
        , clicks (\pos ->
            toLocal model.size pos
                |> Maybe.map (\pos -> Pause)
                |> Maybe.withDefault NoOp
          )
        , moves (\pos ->
            toLocal model.size pos
                |> Maybe.map (\localPos -> Locate localPos)
                |> Maybe.withDefault NoOp
          )
        , rotate Rotate
        , changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
          )
        , configureLorenz (\lorenzConfig ->
            configureFirst model (LorenzConfig lorenzConfig) (\layer ->
                case layer of
                    LorenzLayer _ _ _ -> True
                    _ -> False
            )
          )
        , configureFss (\fssConfig ->
            configureFirst model (FssConfig fssConfig) (\layer ->
                case layer of
                    FssLayer _ _ _ _ -> True
                    MirroredFssLayer _ _ _ _ _ -> True
                    _ -> False
            )
          )
        , rebuildFss (\serializedMesh ->
            RebuildFss serializedMesh
          )
        , pause (\_ -> Pause)
        , start (\_ -> Start)
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
                config.mirror
                config.clip
                serialized
                [ DepthTest.default, Blend.produce blend, sampleAlphaToCoverage ]
                fss
            ]
        MirroredFssLayer config mirror blend serialized fss ->
            let
                config1 = { config | clip = ( mirror.mirrorPos, 1 ) }
                config2 = { config | clip = ( 0, mirror.mirrorPos ), mirror = True }
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
        (
        --span [ class "fps" ] [ toString model.fps ++ "FPS" |> text ]
        --    :: Html.map mapControls
        --     (config |>
        --           Controls.controls numVertices theta)
           --:: WebGL.toHtmlWith
           WebGL.toHtmlWith
              [ WebGL.antialias
              , WebGL.alpha True
              , WebGL.clearColor 0.0862745098 0.0862745098 0.0862745098 1.0
              --, WebGL.depth 0.5
              ]
              [ width (Tuple.first model.size)
              , height (Tuple.second model.size)
              , style [ ( "display", "block" ), ("background-color", "#161616") ]
              ]
              (mergeLayers model)
          :: []
        )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port pause : (() -> msg) -> Sub msg

port start : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port configureLorenz : (Lorenz.Config -> msg) -> Sub msg

port configureFss : (FSS.Config -> msg) -> Sub msg

-- TODO: port to affect camera

port rebuildFss : (FSS.SerializedScene -> msg) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : Blend
      }
    -> msg) -> Sub msg
