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


import Controls
import Lorenz
import Fractal
import Triangle
import Blend exposing (Blend)


type alias LayerIndex = Int


type LayerConfig
    = NoConfig
    | LorenzConfig Lorenz.Config
    | FractalConfig Fractal.Config


type Layer
    = LorenzLayer Blend Lorenz.LorenzMesh
    | FractalLayer Blend Fractal.FractalMesh
    | TriangleLayer Blend Triangle.TriangleMesh
    | TextLayer Blend
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
    | Rotate Float
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        lorenzConfig = Lorenz.init
        fractalConfig = Fractal.init
    in
        (
            { paused = False
            , autoRotate = True
            , fps = 0
            , theta = 0.1
            , layers = Array.fromList
                [ LorenzLayer Blend.default (lorenzConfig |> Lorenz.build)
                -- , FractalLayer Blend.default (fractalConfig |> Fractal.build)
                , LorenzLayer Blend.default (lorenzConfig |> Lorenz.build)
                , TriangleLayer Blend.default Triangle.mesh
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

        Configure index maybeConfig ->
            let
                curBlend = model.layers
                    |> Array.get index
                    |> Maybe.map getBlend
                    |> Maybe.withDefault Blend.default
                layer = case maybeConfig of
                    NoConfig ->
                        TriangleLayer curBlend Triangle.mesh
                    LorenzConfig lorenzConfig ->
                        LorenzLayer curBlend (lorenzConfig |> Lorenz.build)
                    FractalConfig fractalConfig ->
                        FractalLayer curBlend (fractalConfig |> Fractal.build)
            in
                ( { model
                  | layers = model.layers
                      |> Array.set index layer
                  }
                  , Cmd.none
                )

        ChangeBlend index newBlend ->
            case model.layers |> Array.get index of
                Just layer ->
                    let
                        newLayer =
                            case layer of
                                TriangleLayer _ mesh ->
                                    TriangleLayer newBlend mesh
                                LorenzLayer _ mesh ->
                                    LorenzLayer newBlend mesh
                                FractalLayer _ mesh ->
                                    FractalLayer newBlend mesh
                                TextLayer _ ->
                                    TextLayer newBlend
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


getBlend : Layer -> Blend
getBlend layer =
    case layer of
        LorenzLayer blend _ -> blend
        FractalLayer blend _ -> blend
        TriangleLayer blend _ -> blend
        TextLayer blend -> blend


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        , rotate Rotate
        , modify (\lorenzConfig ->
            Configure 0 (LorenzConfig lorenzConfig)
          )
        , changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
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
    layers |> Array.indexedMap
        (\index layer ->
            case layer of
                LorenzLayer blend lorenz ->
                    Lorenz.makeEntity
                        (theta * (toFloat index + 1))
                        [ DepthTest.default, Blend.produce blend ]
                        lorenz
                FractalLayer blend fractal ->
                    Fractal.makeEntity
                        [ DepthTest.default, Blend.produce blend ]
                        fractal
                TriangleLayer blend _ ->
                    Triangle.entity theta [ DepthTest.default, Blend.produce blend ]
                TextLayer blend ->
                    -- FIXME: replace with text
                    Triangle.entity theta [ DepthTest.default, Blend.produce blend ]
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

port modify : (Lorenz.Config -> msg) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : Blend
      }
    -> msg) -> Sub msg
