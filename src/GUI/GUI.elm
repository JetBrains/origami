module GUI exposing (..)

import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
    exposing (class, width, height, style, class, type_, min, max, value, id)
import Html.Events exposing (on, onInput, onMouseUp, onClick)

import Array exposing (..)


type alias BlockSize = (Int, Int)


type alias Shift = Int


type Grid = Grid Shift (Array (Array Cell))


type ExpandState
    = Expanded
    | Collapsed


-- type ExpandDirection
--     = Up
--     | Down


type ToggleState
    = TurnedOn
    | TurnedOff


type alias Coord = ( Int, Int )


type alias Handler = (() -> ())


type Cell
    = Knob Float
    | Toggle ToggleState
    | Button Handler
    | Nested ExpandState Grid
    | Choice Coord Grid
    -- | Color


type Msg
    = Tune Path Float
    | On Path
    | Off Path
    | Click Path
    | Expand Path
    | Collapse Path
    | Choose Path Int Int
    | Move Int Int


type alias UI = Grid


type alias Path = Array Coord


grid : Shift -> List (List Cell) -> Grid
grid shift cells =
    Grid shift <| Array.fromList (List.map Array.fromList cells)


emptyGrid : Shift -> Grid
emptyGrid shift
    = grid shift []


init : ( UI, Cmd Msg )
init = ( emptyGrid 0, Cmd.none )


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
