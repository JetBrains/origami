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
import Triangle
import Blend exposing (Blend, produce, default)


type Layer
    = LorenzLayer Blend Lorenz.LorenzMesh
    | TriangleLayer Blend Triangle.TriangleMesh
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
    | ModifyLayer Int Blend (Maybe Lorenz.Config)
    | Rotate Float
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        lorenzConfig = Lorenz.init
    in
        (
            { paused = False
            , autoRotate = True
            , fps = 0
            , theta = 0.1
            , layers = Array.fromList
                [ LorenzLayer default (lorenzConfig |> Lorenz.build)
                , TriangleLayer default Triangle.mesh
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

        ModifyLayer index blend layerConfig ->
            let
                layer = case layerConfig of
                    Just lorenzConfig ->
                        LorenzLayer blend (lorenzConfig |> Lorenz.build)
                    Nothing ->
                        TriangleLayer blend Triangle.mesh
            in
                ( { model
                  | layers = model.layers
                      |> Array.set index layer
                  }
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

        _ -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        , rotate Rotate
        , modify (\lorenzConfig ->
            ModifyLayer 0 default (Just lorenzConfig)
          )
        , pause (\_ -> Pause)
        , start (\_ -> Start)
        ]


mapControls : Model -> Controls.Msg -> Msg
mapControls model controlsMsg =
    case controlsMsg of
        Controls.ChangeConfig cfg -> ModifyLayer 0 default (Just cfg)
        Controls.Rotate th -> Rotate th


mergeLayers : Float -> Array Layer -> List WebGL.Entity
mergeLayers theta layers =
    Array.toList
        (layers |> Array.indexedMap
            (\index layer ->
                case ( index, layer )  of
                    ( 0, LorenzLayer blend lorenz ) ->
                        Lorenz.makeEntity ( theta * 2 ) [ DepthTest.default, produce blend ] lorenz
                    ( _, LorenzLayer blend lorenz ) ->
                        Lorenz.makeEntity theta [ DepthTest.default, produce blend ] lorenz
                    ( _, TriangleLayer _ _ ) -> Triangle.entity theta
            )
        )


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
