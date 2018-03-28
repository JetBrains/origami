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
    | TextLayer Blend
    | Unknown
    -- | CanvasLayer (\_ -> )


type alias Model =
    { paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , layers : Array Layer
    , size : ( Int, Int )
  --  , mouse : Maybe Position
    , now : Time
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | Configure LayerIndex LayerConfig
    | ChangeBlend LayerIndex Blend
    | RebuildFss FSS.SerializedScene
    | Rotate Float
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        lorenzConfig = Lorenz.init
        -- fractalConfig = Fractal.init
        -- voronoiConfig = Voronoi.init
        templateConfig = Template.init
        fssConfig = FSS.init
    in
        (
            { paused = False
            , autoRotate = False
            , fps = 0
            , theta = 0.1
            , layers = Array.fromList
                --[ TemplateLayer templateConfig Blend.default (templateConfig |> Template.build)
                [ FssLayer fssConfig Blend.default Nothing (FSS.build fssConfig Nothing )
                --, LorenzLayer lorenzConfig Blend.default (lorenzConfig |> Lorenz.build)
                --, FractalLayer Blend.default (fractalConfig |> Fractal.build)
                --, VoronoiLayer Blend.default (voronoiConfig |> Voronoi.build)
                ]
            , size = ( 1500, 800 )
            , now = 0.0
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
            case model.layers |> Array.get index of
                Just layer ->
                    let
                        newLayer = case ( layer, config ) of
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
                            ( TemplateLayer _ curBlend _, TemplateConfig templateConfig ) ->
                                TemplateLayer templateConfig curBlend (templateConfig |> Template.build)
                            _ -> Unknown
                    in
                        ( { model
                          | layers = model.layers
                              |> Array.set index newLayer
                          }
                        , Cmd.none
                        )
                Nothing -> ( model, Cmd.none )

        ChangeBlend index newBlend ->
            case model.layers |> Array.get index of
                Just layer ->
                    let
                        newLayer =
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
                                TextLayer _ ->
                                    TextLayer newBlend
                                _ -> Unknown
                    in
                        ( { model
                           | layers = model.layers
                               |> Array.set index newLayer
                           }
                           , Cmd.none
                           )
                Nothing ->
                    ( model , Cmd.none )

        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )

        Resize { width, height } ->
            ( model
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
                Array.map (\layer ->
                    case f layer of
                        Just newLayer -> newLayer
                        Nothing -> layer
                )
    }


configureFirst : Model -> LayerConfig -> (Layer -> Bool) -> Msg
configureFirst { layers } config f =
    layers
        |> Array.foldl
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
        |> (\idx -> Configure idx config)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        , clicks (\_ -> Pause)
        , rotate Rotate
        , changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
        )
        , modifyLorenz (\lorenzConfig ->
            configureFirst model (LorenzConfig lorenzConfig) (\layer ->
                case layer of
                    LorenzLayer _ _ _ -> True
                    _ -> False
            )
        )
        , changeFss (\fssConfig ->
            configureFirst model (FssConfig fssConfig) (\layer ->
                case layer of
                    FssLayer _ _ _ _ -> True
                    _ -> False
            )
        )
        , receiveFss (\serializedMesh ->
            RebuildFss serializedMesh
        )
        , pause (\_ -> Pause )
        , start (\_ -> Start)
        ]



mapControls : Model -> Controls.Msg -> Msg
mapControls model controlsMsg =
    case controlsMsg of
        Controls.Configure cfg -> Configure 0 (LorenzConfig cfg)
        Controls.Rotate th -> Rotate th


mergeLayers : Float -> Time -> ( Int, Int ) -> Array Layer -> List WebGL.Entity
mergeLayers theta now size layers =
    let viewport = Viewport.find { theta = theta, size = size }
    in layers |> Array.map
        (\layer ->
            case layer of
                -- FIXME: simplify
                LorenzLayer _ blend lorenz ->
                    Lorenz.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        lorenz
                FractalLayer _ blend fractal ->
                    Fractal.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        fractal
                TemplateLayer _ blend template ->
                    Template.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        template
                VoronoiLayer _ blend voronoi ->
                    Voronoi.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        voronoi
                FssLayer _ blend serialized fss ->
                    FSS.makeEntity
                        viewport
                        now
                        serialized
                        [ DepthTest.default, Blend.produce blend, sampleAlphaToCoverage ]
                        fss
                TextLayer blend ->
                    -- FIXME: replace with text
                    Template.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        (Template.init |> Template.build)
                Unknown ->
                    Template.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce Blend.default ]
                        (Template.init |> Template.build)
        )
    |> Array.toList


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
              (model.layers |> mergeLayers model.theta model.now model.size)
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

port modifyLorenz : (Lorenz.Config -> msg) -> Sub msg

-- TODO: port to affect camera

port changeFss : (FSS.Config -> msg) -> Sub msg

port receiveFss : (FSS.SerializedScene -> msg) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : Blend
      }
    -> msg) -> Sub msg
