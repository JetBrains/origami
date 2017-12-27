module Main exposing (main)

import Html exposing (Html)
import AnimationFrame

import Models exposing (..)
import Msgs exposing (..)
import View exposing (..)
import Update exposing (..)
import Ports exposing (..)
import Window
import Task exposing (Task)


init : ( Model, Cmd Msg )
init =
    let
        numVertices = 2000
        lorenzConfig =
            { sigma = 10
            , beta = 8 / 3
            , rho = 28
            , stepSize = 0.005
            }
    in
        (
            { config = lorenzConfig
            , paused = False
            , fps = 0
            , theta = 0.1
            , lorenz = lorenzConfig |> lorenz numVertices
            , numVertices = numVertices
            }
        , Cmd.batch
            [ Task.perform Resize Window.size
            ]
        )

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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

