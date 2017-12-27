port module Main exposing (main)

import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Time exposing (Time)
import Window
import Task exposing (Task)
import WebGL exposing (Mesh)


import Controls
import Lorenz


type alias Model =
    { config : Lorenz.Config
    , paused : Bool
    , fps : Int
    , theta : Float
    , lorenz : Mesh Lorenz.Vertex
    , numVertices : Int
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | ChangeConfig Lorenz.Config
    | AdjustVertices Int
    | Rotate Float
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        numVertices = 2000
        lorenzConfig = Lorenz.init
    in
        (
            { config = lorenzConfig
            , paused = False
            , fps = 0
            , theta = 0.1
            , lorenz = lorenzConfig |> Lorenz.build numVertices
            , numVertices = numVertices
            }
        , Cmd.batch
            [ Task.perform Resize Window.size
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model | fps = floor (1000 / dt), theta = model.theta +  dt / 4000 }
            , Cmd.none
            )

        AdjustVertices verticesCount ->
            ( { model
              | numVertices = verticesCount
              , lorenz = model.config
                |> Lorenz.build model.numVertices }
            , Cmd.none
            )
        ChangeConfig newConfig ->
            ( { model
              | config = newConfig
              , lorenz = newConfig
                |> Lorenz.build model.numVertices
              }
            , Cmd.none
            )
        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )
        _ -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        , rotate Rotate
        , modify ChangeConfig
        , pause (\_ -> Pause)
        , start (\_ -> Start)
        ]


mapControls : Controls.Msg -> Msg
mapControls controlsMsg =
    case controlsMsg of
        Controls.AdjustVertices n -> AdjustVertices n
        Controls.ChangeConfig cfg -> ChangeConfig cfg
        Controls.Rotate th -> Rotate th


view : Model -> Html Msg
view { config, lorenz, numVertices, theta, fps } =
    div [ ]
        ( text (toString fps ++ "FPS")
          :: Html.map mapControls
                (config |>
                    Controls.controls numVertices theta)
          :: WebGL.toHtml
              [ width 1550
              , height 800
              , style [ ( "display", "block" ) ]
              ]
              [ Lorenz.makeEntity
                  lorenz
                  theta
              ]
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
