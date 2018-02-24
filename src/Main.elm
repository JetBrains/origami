port module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html, text, div, span)
import Html.Attributes exposing (width, height, style, class)
import AnimationFrame
import Time exposing (Time)
import Window
import Task exposing (Task)
import WebGL exposing (Mesh)
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
    = LorenzLayer Blend Lorenz.Mesh
    | FractalLayer Blend Fractal.Mesh
    | VoronoiLayer Blend Voronoi.Mesh
    | TemplateLayer Blend Template.Mesh
    | FssLayer Blend (Maybe FSS.SerializedScene) FSS.Mesh
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
        fractalConfig = Fractal.init
        voronoiConfig = Voronoi.init
        templateConfig = Template.init
    in
        (
            { paused = False
            , autoRotate = True
            , fps = 0
            , theta = 0.1
            , layers = Array.fromList
                -- [ FractalLayer Blend.default (fractalConfig |> Fractal.build)
                [ VoronoiLayer Blend.default (voronoiConfig |> Voronoi.build)
                --, LorenzLayer Blend.default (lorenzConfig |> Lorenz.build)
                , TemplateLayer Blend.default (templateConfig |> Template.build)
                ]
            , size = ( 0, 0 )
            }
        , Cmd.batch
            [ Task.perform Resize Window.size
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( if model.autoRotate then
                { model
                | fps = floor (1000 / dt)
                , theta = model.theta + dt / 4000
                }
              else
                { model
                | fps = floor (1000 / dt)
                }
            , Cmd.none
            )

        Configure index config ->
            case model.layers |> Array.get index of
                Just layer ->
                    let
                        newLayer = case ( layer, config ) of
                            -- FIXME: simplify
                            ( LorenzLayer curBlend _, LorenzConfig lorenzConfig ) ->
                                LorenzLayer curBlend (lorenzConfig |> Lorenz.build)
                            ( FractalLayer curBlend _, FractalConfig fractalConfig ) ->
                                FractalLayer curBlend (fractalConfig |> Fractal.build)
                            ( VoronoiLayer curBlend _, VoronoiConfig voronoiConfig ) ->
                                VoronoiLayer curBlend (voronoiConfig |> Voronoi.build)
                            ( FssLayer curBlend maybeScene _, FssConfig fssConfig ) ->
                                FssLayer curBlend maybeScene (maybeScene |> FSS.build fssConfig)
                            ( TemplateLayer curBlend _, TemplateConfig templateConfig ) ->
                                TemplateLayer curBlend (templateConfig |> Template.build)
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
                                TemplateLayer _ mesh ->
                                    TemplateLayer newBlend mesh
                                LorenzLayer _ mesh ->
                                    LorenzLayer newBlend mesh
                                FractalLayer _ mesh ->
                                    FractalLayer newBlend mesh
                                VoronoiLayer _ mesh ->
                                    VoronoiLayer newBlend mesh
                                FssLayer _ scene mesh ->
                                    FssLayer newBlend scene mesh
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
            ( { model | size = ( width, height ) }
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
        , rotate Rotate
        , changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
        )
        , modifyLorenz (\lorenzConfig ->
            configureFirst model (LorenzConfig lorenzConfig) (\layer ->
                case layer of
                    LorenzLayer _ _ -> True
                    _ -> False
            )
        )
        , changeFss (\fssConfig ->
            configureFirst model (FssConfig fssConfig) (\layer ->
                case layer of
                    FssLayer _ _ _ -> True
                    _ -> False
            )
        )
        , receiveFss (\serializedMesh ->
            RebuildFss serializedMesh
        )
        , pause (\_ -> Pause)
        , start (\_ -> Start)
        ]


mapControls : Model -> Controls.Msg -> Msg
mapControls model controlsMsg =
    case controlsMsg of
        Controls.Configure cfg -> Configure 0 (LorenzConfig cfg)
        Controls.Rotate th -> Rotate th


mergeLayers : Float -> Array Layer -> List WebGL.Entity
mergeLayers theta layers =
    let viewport = Viewport.find { theta = theta }
    in layers |> Array.map
        (\layer ->
            case layer of
                -- FIXME: simplify
                LorenzLayer blend lorenz ->
                    Lorenz.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        lorenz
                FractalLayer blend fractal ->
                    Fractal.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        fractal
                TemplateLayer blend template ->
                    Template.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        template
                VoronoiLayer blend voronoi ->
                    Voronoi.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
                        voronoi
                FssLayer blend serialized fss ->
                    FSS.makeEntity
                        viewport
                        [ DepthTest.default, Blend.produce blend ]
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
        ( span [ class "fps" ] [ toString model.fps ++ "FPS" |> text ]
        --    :: Html.map mapControls
        --     (config |>
        --           Controls.controls numVertices theta)
           :: WebGL.toHtml
              [ width (Tuple.first model.size)
              , height (Tuple.second model.size)
              , style [ ( "display", "block" ), ("background-color", "#12181C") ]
              ]
              (model.layers |> mergeLayers model.theta)
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
