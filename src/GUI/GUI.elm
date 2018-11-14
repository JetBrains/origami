module GUI exposing (..)

import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
    exposing (class, width, height, style, class, type_, min, max, value, id)
import Html.Events exposing (on, onInput, onMouseUp, onClick)

import Array exposing (..)


type Cell
    = Knob Float
    | Toggle Bool
    | Button (() -> ())
    | Nested Bool (Array Cell)
    | Choice Int (Array Cell)
    -- | Color

type Msg
    = Tune Path Float
    | On Path
    | Off Path
    | Click Path
    | Expand Path
    | Collapse Path
    | Choose Path Int Int


type alias UI = Array Cell


type alias Path = Array Int


init : ( UI, Cmd Msg )
init = ( Array.fromList [], Cmd.none )


view : UI -> Html Msg
view ui = div [] []


subscriptions : UI -> Sub Msg
subscriptions ui = Sub.batch []


update : Msg -> UI -> ( UI, Cmd Msg )
update msg ui = ( ui, Cmd.none )


main : Program Never UI Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
