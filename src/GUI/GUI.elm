module Gui.Gui exposing
    ( Msg
    , UI
    , view
    , update
    , init
    )

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


type alias Label = String


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState Grid
    | Choice Label Coord Grid
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


-- TODO:
-- initialMode : UiMode
-- initialMode = Development


type alias UI = Grid


type alias Path = Array Coord


emptyGrid : Shift -> Grid
emptyGrid shift
    = grid shift []


grid : Shift -> List (List Cell) -> Grid
grid shift cells =
    Grid shift <| Array.fromList (List.map Array.fromList cells)


oneLine : Shift -> List Cell -> Grid
oneLine shift cells =
    grid shift [cells]


init : UI -- ( UI, Cmd Msg )
init =
    let
        webglBlendGrid = emptyGrid 0
        svgBlendGrid = emptyGrid 0
        amplitudeGrid = emptyGrid 0
        fssControls shift =
            oneLine shift
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOn
                , Knob "lights" 0
                , Knob "col" 0
                , Knob "vignette" 0
                , Knob "iris" 0
                , Choice "mesh" (0, 0) <| emptyGrid 0
                , Nested "amplitude" Collapsed amplitudeGrid
                , Nested "blend" Collapsed webglBlendGrid
                ]
        svgControls shift =
            oneLine shift
                [ Toggle "visible" TurnedOn
                , Nested "blend" Collapsed svgBlendGrid
                ]
    in
        oneLine 0
            [ Choice "product" (0, 0) <| emptyGrid 0
            , Knob "rotation" 0
            , Choice "size" (0, 0) <| emptyGrid 0
            , Button "save png" (\_ -> ())
            , Button "save batch" (\_ -> ())
            , Nested "logo" Collapsed <| svgControls 0
            , Nested "title" Collapsed <| svgControls 0
            , Nested "net" Collapsed <| fssControls 0
            , Nested "low-poly" Collapsed <| fssControls 0
            ]


view : UI -> Html Msg
view (Grid shift cells) =
    div [ H.class "gui" ] []


subscriptions : UI -> Sub Msg
subscriptions ui = Sub.batch []


update : Msg -> UI -> UI -- ( UI, Cmd Msg )
update msg ui = ui -- ( ui, Cmd.none )


-- main : Program Never UI Msg
-- main =
--     Html.program
--         { init = init
--         , view = view
--         , subscriptions = subscriptions
--         , update = update
--         }
