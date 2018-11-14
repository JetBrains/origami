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


type alias CellPos = ( Int, Int )


type Origin = Origin CellPos


type Cells = Cells (Array (Array Cell))


type Grid = Grid Origin Cells


type ExpandState
    = Expanded
    | Collapsed


-- type ExpandDirection
--     = Up
--     | Down


type ToggleState
    = TurnedOn
    | TurnedOff


type alias Handler = (() -> ())


type alias Label = String


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState Grid
    | Choice Label CellPos Grid
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


type alias Path = Array CellPos


emptyGrid : Origin -> Grid
emptyGrid origin
    = grid origin []


grid : Origin -> List (List Cell) -> Grid
grid origin cells =
    Grid origin
        <| Cells
        <| Array.fromList
        <| List.map Array.fromList cells


oneLine : Origin -> List Cell -> Grid
oneLine origin cells =
    grid origin [cells]


init : UI -- ( UI, Cmd Msg )
init =
    let
        webglBlendGrid y = emptyGrid <| Origin ( 0, y )
        svgBlendGrid y = emptyGrid <| Origin ( 0, y )
        amplitudeGrid y = emptyGrid <| Origin ( 0, y )
        fssControls (Origin (x, y) as origin) =
            oneLine origin
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOn
                , Knob "lights" 0
                , Knob "col" 0
                , Knob "vignette" 0
                , Knob "iris" 0
                , Choice "mesh" (0, 0) <| emptyGrid <| Origin ( x, y + 1 )
                , Nested "amplitude" Collapsed <| amplitudeGrid (y + 1)
                , Nested "blend" Collapsed <| webglBlendGrid (y + 1)
                ]
        svgControls (Origin (x, y) as origin) =
            oneLine origin
                [ Toggle "visible" TurnedOn
                , Nested "blend" Collapsed <| svgBlendGrid y
                ]
    in
        oneLine (Origin (0, 0))
            [ Choice "product" (0, 0) <| emptyGrid <| Origin (0, 1)
            , Knob "rotation" 0
            , Choice "size" (0, 0) <| emptyGrid <| Origin (2, 1)
            , Button "save png" (\_ -> ())
            , Button "save batch" (\_ -> ())
            , Nested "logo" Collapsed
                <| svgControls <| Origin (0, 1)
            , Nested "title" Collapsed
                <| svgControls <| Origin (0, 1)
            , Nested "net" Collapsed
                <| fssControls <| Origin (0, 1)
            , Nested "low-poly" Collapsed
                <| fssControls <| Origin (0, 1)
            ]

viewHole : Html Msg
viewHole =
    div [ H.class "cell hole" ] []


viewCell : Int -> Int -> Cell -> Html Msg
viewCell rowIndex cellIndex cell =
    div
        [ H.class "cell" ]
        [ case cell of
            Knob label val ->
                span [ ] [ text <| "knob: " ++ label ++ " " ++ toString val ]
            Toggle label val ->
                span [ ]
                    [ text <| "toggle: " ++ label ++ " "
                      ++ (if val == TurnedOn then "on" else "off")
                    ]
            Button label _ ->
                span [ ]
                    [ text <| "button: " ++ label ]
            Nested label state _ ->
                span [ ]
                    [ text <| "toggle: " ++ label ++ " "
                      ++ (if state == Expanded then "expanded" else "collapsed")
                    ]
            Choice label (x, y) _ ->
                span [ ]
                    [ text <| "choice: " ++ label ++ " "
                      ++ toString x ++ " " ++ toString y
                    ]
        ]


viewCellRow : Origin -> Int -> Array Cell -> Html Msg
viewCellRow (Origin (x, y)) rowIndex cells =
    div [ H.class "row" ]
        <| List.repeat x viewHole ++
            ( Array.indexedMap (viewCell rowIndex) cells
                |> Array.toList
            )


viewCells : Origin -> Cells -> Html Msg
viewCells origin (Cells cells) =
    div [ H.class "cells" ]
        <| Array.toList
        <| Array.indexedMap (viewCellRow origin) cells



viewGrid : Grid -> Html Msg
viewGrid (Grid shift cells) =
    div [ H.class "grid" ]
        [ viewCells shift cells ]


view : UI -> Html Msg
view ui =
    div [ H.class "gui" ] [ viewGrid ui ]


subscriptions : UI -> Sub Msg
subscriptions ui = Sub.batch []


update : Msg -> UI -> UI -- ( UI, Cmd Msg )
update msg ui = ui -- ( ui, Cmd.none )
