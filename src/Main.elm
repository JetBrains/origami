port module Main exposing (main)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (width, height, style, class)
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
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , lorenz : Lorenz.LorenzMesh
    , numVertices : Int
    , size : ( Int, Int )
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
            , autoRotate = True
            , fps = 0
            , theta = 0.1
            , lorenz = lorenzConfig |> Lorenz.build numVertices
            , numVertices = numVertices
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

        Resize { width, height } ->
            ( { model | size = ( width, height ) }
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
              [ Lorenz.makeEntity
                  model.lorenz
                  model.theta
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
