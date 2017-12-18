module Main exposing (main)

import AnimationFrame
import Html exposing (Html, text)
import Task exposing (Task)
import Time exposing (Time)
import Window


type alias Model =
    { sigma : Float
    , beta : Float
    , rho : Float
    , stepSize : Float
    , stepsPerFrame : Int
    , paused : Bool
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | Pause
    | Start

init : ( Model, Cmd Msg )
init =
    (
        { sigma = 10
        , beta = 8 / 3
        , rho = 28
        , stepSize = 0.002
        , stepsPerFrame = 3
        , paused = False
        }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )

view : Model -> Html Msg
view model =
    let
        a = "foo"
    in
        text (toString model.sigma ++ a)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model | sigma = dt}
            , Cmd.none
            )
        _ -> ( model, Cmd.none )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
