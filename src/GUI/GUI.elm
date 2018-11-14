module Gui.Gui exposing
    ( Msg
    , UI
    , view
    , update
    , init
    )


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H


type alias CellPos = ( Int, Int )


type alias Bounds = ( Int, Int )


type alias Rows = Array Row


type alias Row = Array Cell


--type Grid = Grid Origin Cells
type Grid = Grid Rows


type ExpandState
    = Expanded
    | Collapsed


-- type ExpandDirection
--     = Up
--     | Down


type ToggleState
    = TurnedOn
    | TurnedOff


type SelectionState
    = Selected
    | NotSelected


type alias Handler = (() -> ())


type alias Label = String


type Cell
    = Empty
    | Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState Grid
    | Choice Label CellPos Grid
    | ChoiceItem SelectionState Cell
    -- | Color


type Msg
    = Tune CellPos Float
    | On CellPos
    | Off CellPos
    | Expand CellPos
    | Collapse CellPos
    | Choose CellPos CellPos
    | Move CellPos Int


-- TODO:
-- initialMode : UiMode
-- initialMode = Development


type alias UI = Grid


type alias Path = CellPos


emptyGrid : Bounds -> Grid
emptyGrid ( width, height )
    = grid
        <| List.repeat height (List.repeat width Empty)


grid : List (List Cell) -> Grid
grid cells =
    Grid
        <| Array.fromList
        <| List.map Array.fromList cells


put : CellPos -> Grid -> Grid -> Grid
put (rowId, colId) (Grid srcRows) (Grid dstRows) =
    let
        updateCell dstRowId dstColId cell =
            if (dstRowId >= rowId) && (dstColId >= colId) then
                srcRows |> getCell_ (dstRowId - rowId, dstColId - colId) cell
            else cell
        updateRow dstRowId row =
            row |> Array.indexedMap (updateCell dstRowId)
        applyIfExpands srcRowId cell ( srcColId, grid ) =
            ( srcColId + 1
            , case cell of
                Nested _ _ nestedGrid ->
                    put ( rowId + srcRowId + 1, colId + srcColId ) nestedGrid grid
                _ -> grid
            )
        checkExpandables row ( srcRowId, grid ) =
            ( srcRowId + 1
            , Array.foldl (applyIfExpands srcRowId) (0, grid) row
                |> Tuple.second
            )
    in
        dstRows
            |> Array.indexedMap updateRow
            |> Grid
            |> (\dstGrid ->
                    Array.foldl checkExpandables (0, dstGrid) srcRows
               )
            |> Tuple.second


oneLine : List Cell -> Grid
oneLine cells =
    grid [cells]


bottomLeft : CellPos
bottomLeft = (0, 0)


init : UI -- ( UI, Cmd Msg )
init =
    let
        webglBlendGrid = emptyGrid ( 0, 0 )
        svgBlendGrid = emptyGrid ( 0, 0 )
        amplitudeGrid = emptyGrid ( 0, 0 )
        fssControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOn
                , Knob "lights" 0
                , Knob "col" 0
                , Knob "vignette" 0
                , Knob "iris" 0
                , Choice "mesh" (0, 0) <| emptyGrid ( 0, 0 )
                , Nested "amplitude" Collapsed amplitudeGrid
                , Nested "blend" Collapsed webglBlendGrid
                ]
        svgControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Nested "blend" Collapsed svgBlendGrid
                ]
        bottomLine =
            oneLine
                [ Choice "product" (0, 0)
                    <| emptyGrid ( 0, 0 )
                , Knob "rotation" 0
                , Choice "size" (0, 0)
                    <| emptyGrid ( 0, 0 )
                , Button "save png" (\_ -> ())
                , Button "save batch" (\_ -> ())
                , Nested "logo" Collapsed svgControls
                , Nested "title" Collapsed svgControls
                , Nested "net" Collapsed fssControls
                , Nested "low-poly" Collapsed fssControls
                ]
    in
        put bottomLeft bottomLine <| emptyGrid (10, 10)


showPos : CellPos -> String
showPos (row, col) =
    "(" ++ toString row ++ "," ++ toString col ++ ")"


handleClick : CellPos -> Cell -> Maybe Msg
handleClick (( row, col ) as pos) cell =
    case cell of
        Toggle _ val -> Just (if val == TurnedOn then Off pos else On pos)
        Nested _ state _ -> Just (if state == Expanded then Collapse pos else Expand pos)
        _ -> Nothing


viewCell_ : CellPos -> Cell -> Html Msg
viewCell_ (( row, col ) as pos) cell =
    case cell of
        Empty -> span [] []
        Knob label val ->
            span [ ] [ text <| showPos pos ++ " knob: " ++ label ++ " " ++ toString val ]
        Toggle label val ->
            span [ ]
                [ text <| showPos pos ++ " toggle: " ++ label ++ " "
                    ++ (if val == TurnedOn then "on" else "off")
                ]
        Button label _ ->
            span [ ]
                [ text <| showPos pos ++ " button: " ++ label ]
        Nested label state _ ->
            span [ ]
                [ text <| showPos pos ++ " toggle: " ++ label ++ " "
                    ++ (if state == Expanded then "expanded" else "collapsed")
                ]
        Choice label (x, y) _ ->
            span [ ]
                [ text <| showPos pos ++ " choice: " ++ label ++ " "
                    ++ toString x ++ " " ++ toString y
                ]
        ChoiceItem state cell ->
            span []
                [ text <| if state == Selected then "selected" else "not selected"
                , viewCell_ pos cell
                ]



viewCell : CellPos -> Cell -> Html Msg
viewCell pos cell =
    let
        className =
            case cell of
                Empty -> "cell hole"
                _ -> "cell"
        handlers =
            handleClick pos cell
            |> Maybe.map (\msg -> [ H.onClick msg ])
            |> Maybe.withDefault []
    in
        div
            ([ H.class className ]
            ++ handlers)
            [ viewCell_ pos cell
            ]


viewRow : CellPos -> Array Cell -> Html Msg
viewRow (row, col) cols =
    Array.indexedMap
        (\subCol -> viewCell ( row, col + subCol ))
        cols
        |> Array.toList
        |> div [ H.class "row" ]


viewRows : Rows -> Html Msg
viewRows rows =
    let
        origin  = bottomLeft
        ( row, col ) = origin
        topRows =
            rows
                |> Array.indexedMap
                    (\subRow -> viewRow (row + subRow, col))
                |> Array.toList
    in
        topRows |> div [ H.class "cells" ]



viewGrid : Grid -> Html Msg
viewGrid (Grid grid) =
    div [ H.class "grid" ]
        [ viewRows grid ]


view : UI -> Html Msg
view ui =
    div [ H.class "gui" ] [ viewGrid ui ]


getCell : CellPos -> Cell -> Grid -> Cell
getCell pos default (Grid rows) =
    getCellSafe pos rows |> Maybe.withDefault default


getCell_ : CellPos -> Cell -> Rows -> Cell
getCell_ pos default rows =
   getCellSafe pos rows |> Maybe.withDefault default


getCellSafe : CellPos -> Rows -> Maybe Cell
getCellSafe ( row, col ) rows =
    rows
        |> Array.get row
        |> Maybe.andThen (Array.get col)


updateCell : CellPos -> (Cell -> Cell) -> Grid -> Grid
updateCell ( row, col ) f (Grid rows) =
    case getCellSafe ( row, col ) rows of
        Just prevCell ->
            Array.get row rows
                |> Maybe.map (Array.set col prevCell)
                |> Maybe.map (\newRow -> Array.set row newRow rows)
                |> Maybe.withDefault rows
                |> Grid
        Nothing -> Grid rows


subscriptions : UI -> Sub Msg
subscriptions ui = Sub.batch []


update : Msg -> UI -> UI -- ( UI, Cmd Msg )
update msg ui =
    case msg of
        On pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOn
                            _ -> cell
                    )
        Off pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOff
                            _ -> cell
                    )
        Expand pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ grid ->
                                Nested label Expanded grid
                            _ -> cell
                    )
        Collapse pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ grid ->
                                Nested label Collapsed grid
                            _ -> cell
                    )
        _ -> ui
