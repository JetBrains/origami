module Gui.Gui exposing
    ( Msg
    , Model
    , view
    , update
    , init
    )


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H


type GridPos = GridPos Int Int
type ModelPos = ModelPos Int Int
type alias Shape = ( Int, Int )

type alias Cells = List Cell
type alias Rows = Array Row
type alias Row = Array (Maybe (Cell, ModelPos))
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
type alias ItemChosen = Int


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState (Shape, Cells)
    | Choice Label ExpandState ItemChosen (Shape, Cells)
    -- | Color


type Msg
    = Tune ModelPos Float
    | On ModelPos
    | Off ModelPos
    | Expand ModelPos
    | Collapse ModelPos
    | Choose ModelPos ModelPos
    | Move ModelPos Int


-- TODO:
-- initialMode : UiMode
-- initialMode = Development


type alias Model = ( Shape, Cells )
type alias View = Grid
--type alias Path = CellPos


emptyGrid : Shape -> Grid
emptyGrid ( width, height )
    = Grid <| Array.repeat height (Array.repeat width Nothing)


noChildren : ( Shape, Cells )
noChildren =
    ( ( 0, 0 ), [] )


-- grid : List (List (Maybe Cell)) -> Grid
-- grid cells =
--     Grid
--         <| Array.fromList
--         <| List.map Array.fromList cells


-- map : (Cell -> Cell) -> Grid -> Grid
-- map f (Grid rows) =
--     Array.map (Array.map f) rows |> Grid


-- mapM : (Cell -> Cell) -> Model -> Model
-- mapM f cell =
--     case cell of
--         Nested label state ( shape, cells ) ->
--             Nested label state ( shape, List.map f cells )
--         Choice label state chosen ( shape, cells ) ->
--             Choice label state chosen ( shape, List.map f cells )
--         _ -> cell


-- put : CellPos -> Grid -> Grid -> Grid
-- put (rowId, colId) (Grid srcRows) (Grid dstRows) =
--     let
--         updateCell dstRowId dstColId cell =
--             if (dstRowId >= rowId) && (dstColId >= colId) then
--                 srcRows |> getCell_ (dstRowId - rowId, dstColId - colId) cell
--             else cell
--         updateRow dstRowId row =
--             row |> Array.indexedMap (updateCell dstRowId)
--         applyIfExpands srcRowId cell ( srcColId, grid ) =
--             ( srcColId + 1
--             , grid |> ensureToExpand ( rowId + srcRowId, colId + srcColId ) cell
--             )
--         checkExpandables row ( srcRowId, grid ) =
--             ( srcRowId + 1
--             , Array.foldl (applyIfExpands srcRowId) (0, grid) row
--                 |> Tuple.second
--             )
--     in
--         dstRows
--             |> Array.indexedMap updateRow
--             |> Grid
--             |> (\dstGrid ->
--                     Array.foldl checkExpandables (0, dstGrid) srcRows
--                )
--             |> Tuple.second

-- root : Shape -> Cells -> Cell
-- root shape cells = Root ( shape, cells )


-- nest : String -> Shape -> Cells -> Cell
-- nest label shape cells =
--     Nested label Collapsed ( shape, cells )


-- choise : String -> Shape -> Cells -> Cell
-- choise label shape cells =
--     Choice label Collapsed 0 ( shape, cells )


oneLine : Cells -> ( Shape, Cells )
oneLine cells =
    ( ( List.length cells, 1 ), cells )


bottomLeft : GridPos
bottomLeft = (GridPos 0 0)


init : Model -- ( UI, Cmd Msg )
init =
    let
        webglBlendGrid = noChildren
        svgBlendGrid = noChildren
        amplitudeGrid = noChildren
        fssControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOn
                , Knob "lights" 0
                , Knob "col" 0
                , Knob "vignette" 0
                , Knob "iris" 0
                , Choice "mesh" Collapsed 0 noChildren
                , Nested "amplitude" Collapsed amplitudeGrid
                , Nested "blend" Collapsed webglBlendGrid
                ]
        svgControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Choice "blend" Collapsed 0 noChildren
                ]
    in
        oneLine
            [ Choice "product" Collapsed 0 noChildren
            , Knob "rotation" 0
            , Choice "size" Collapsed 0 noChildren
            , Button "save png" <| always ()
            , Button "save batch" <| always ()
            , Nested "logo" Collapsed svgControls
            , Nested "title" Collapsed svgControls
            , Nested "net" Collapsed fssControls
            , Nested "low-poly" Collapsed fssControls
            ]


showPos : GridPos -> String
showPos (GridPos row col) =
    "(" ++ toString row ++ "," ++ toString col ++ ")"


findHoverMessage : ModelPos -> Cell -> Maybe Msg
findHoverMessage modelPos cell =
    case cell of
        Knob label value ->
            Tune modelPos (value + 1) |> Just
        _ -> Nothing


findClickMessage : ModelPos -> Cell -> Maybe Msg
findClickMessage modelPos cell =
    case cell of
        Toggle _ val ->
            Just <| if val == TurnedOn then Off modelPos else On modelPos
        Nested _ state _ ->
            Just <| if state == Expanded then Collapse modelPos else Expand modelPos
        _ -> Nothing


viewCell_ : GridPos -> ( Cell, ModelPos ) -> Html Msg
viewCell_ ((GridPos row col) as gridPos) ( cell, _ ) =
    case cell of
        Knob label val ->
            span [ ] [ text <| showPos gridPos ++ " knob: " ++ label ++ " " ++ toString val ]
        Toggle label val ->
            span [ ]
                [ text <| showPos gridPos ++ " toggle: " ++ label ++ " "
                    ++ (if val == TurnedOn then "on" else "off")
                ]
        Button label _ ->
            span [ ]
                [ text <| showPos gridPos ++ " button: " ++ label ]
        Nested label state _ ->
            span [ ]
                [ text <| showPos gridPos ++ " nested: " ++ label ++ " "
                    ++ (if state == Expanded then "expanded" else "collapsed")
                ]
        -- NestedItem level cell ->
        --     span [ ]
        --         [ text <| showPos pos ++ " nested item: " ++ toString level ++ " "
        --         , viewCell_ pos cell
        --         ]
        Choice label selected id _ ->
            span [ ]
                [ text <| showPos gridPos ++ " choice: " ++ label ++ " "
                    ++ toString id
                ]
        -- ChoiceItem level state cell ->
        --     span []
        --         [ text <| (if state == Selected then "selected" else "not selected")
        --             ++ toString level
        --         , viewCell_ pos cell
        --         ]



viewCell : GridPos -> Maybe ( Cell, ModelPos ) -> Html Msg
viewCell gridPos maybeCellPair =
    let
        className =
            case maybeCellPair of
                Just _ -> "cell hole"
                _ -> "cell"
        handlers =
            maybeCellPair
                |> Maybe.map
                    (\(cell, modelPos) ->
                        (findClickMessage modelPos cell
                            |> Maybe.map (\msg -> [ H.onClick msg ])
                            |> Maybe.withDefault []
                        ) ++
                        (findHoverMessage modelPos cell
                            |> Maybe.map (\msg -> [ H.onMouseOver msg ])
                            |> Maybe.withDefault []
                        )
                    )
                |> Maybe.withDefault []
        attributes = [ H.class className ] ++ handlers
        children = maybeCellPair
            |> Maybe.map (\cell -> [ viewCell_ gridPos cell ])
            |> Maybe.withDefault []
    in
        div attributes children


viewRow : GridPos -> Row -> Html Msg
viewRow (GridPos row col) cols =
    Array.indexedMap
        (\subCol -> viewCell (GridPos row (col + subCol)))
        cols
        |> Array.toList
        |> div [ H.class "row" ]


viewRows : Rows -> Html Msg
viewRows rows =
    let
        origin  = bottomLeft
        (GridPos row col) = origin
        topRows =
            rows
                |> Array.indexedMap
                    (\subRow -> viewRow (GridPos (row + subRow) col))
                |> Array.toList
    in
        topRows |> div [ H.class "cells" ]



viewGrid : Grid -> Html Msg
viewGrid (Grid grid) =
    div [ H.class "grid" ]
        [ viewRows grid ]



put : Int -> GridPos -> Shape -> List Cell -> Grid -> Grid
put
    nest
    (GridPos row col)
    shape
    cellsList
    (Grid rows) =
    let
        cells = Array.fromList cellsList
            |> Array.indexedMap (\cellIndex cell -> ( cell, ModelPos nest cellIndex ) )
        indexOf ( row, col ) ( width, _ ) =
            row * width + col
        updateCell row_ col_ prevCell =
            if (row_ >= row) && (col_ >= col) then
                case Array.get (indexOf ( row_, col_ ) shape) cells of
                    Just ( newCell, modelPos ) -> Just ( newCell, modelPos )
                    Nothing -> prevCell
            else prevCell
        updateRow row_ row =
            row |> Array.indexedMap (updateCell row_)
        applyColExpands maybeCell grid =
            case maybeCell of
                Just ( cell, modelPos ) ->
                    case cell of
                        Nested _ Expanded ( shape, cells ) ->
                            put (nest + 1) (GridPos (row + 1) col) shape cells grid
                        Choice _ Expanded _ ( shape, cells ) ->
                            put (nest + 1) (GridPos (row + 1) col) shape cells grid
                        _ -> grid
                _ -> grid
        applyExpands row grid =
            Array.foldl applyColExpands grid row
    in
        rows
            |> Array.indexedMap updateRow
            |> (\rows ->
                    Array.foldl applyExpands (Grid rows) rows
                )


-- put : GridPos -> Cell -> Grid -> Grid
-- put ( x, y ) cell grid =
--     case cell of
--         Root ( shape, cells ) ->
--             grid
--         Knob label val ->
--             grid
--         Toggle label val ->
--             grid
--         Button label _ ->
--             grid
--         Nested label state _ ->
--             grid
--         Choice label selected id _ ->
--             grid


set : GridPos -> ( Cell, ModelPos ) -> Grid -> Grid
set (GridPos row col) cell ((Grid rows) as grid) =
    Array.get row rows
        |> Maybe.map
            (\prevRow ->
                Array.set col (Just cell) prevRow
            )
        |> Maybe.map
            (\newRow ->
                Array.set row newRow rows)
        |> Maybe.map Grid
        |> Maybe.withDefault grid


layout : Model -> Grid
layout ( shape, cells ) =
    emptyGrid (10, 6)
        |> put 0 (GridPos 0 0) shape cells


view : Model -> Html Msg
view model =
    div [ H.class "gui" ]
        [ layout model |> viewGrid ]


-- getCell : CellPos -> Cell -> Grid -> Cell
-- getCell pos default (Grid rows) =
--     getCellSafe pos rows |> Maybe.withDefault default


-- getCell_ : CellPos -> Cell -> Rows -> Cell
-- getCell_ pos default rows =
--    getCellSafe pos rows |> Maybe.withDefault default


-- getCellSafe : CellPos -> Rows -> Maybe Cell
-- getCellSafe ( row, col ) rows =
--     rows
--         |> Array.get row
--         |> Maybe.andThen (Array.get col)


-- getGridShape : Grid -> Shape
-- getGridShape (Grid rows) =
--     ( Array.length rows
--     , Array.foldl
--         (max << Array.length)
--         0
--         rows
--     )


-- ensureToExpand : CellPos -> Cell -> Grid -> Grid
-- ensureToExpand ( row, col ) newCell grid =
--     case newCell of
--         Nested _ Expanded nestedGrid ->
--             put ( row + 1, col ) nestedGrid grid
--         Nested _ Collapsed nestedGrid ->
--             put ( row + 1, col ) (fillEmpty nestedGrid) grid
--         _ -> grid


traverseModel : (Cell -> ModelPos -> Cell) -> Model -> Model
traverseModel f ( shape, cells ) =
    ( shape, traverseCells f cells )


traverseCells : (Cell -> ModelPos -> Cell) -> Cells -> Cells
traverseCells f cells =
    let
        scanCell nest index cell =
            case f cell (ModelPos nest index) of
                Nested label state ( shape, nestedCells ) ->
                    Nested
                        label
                        state
                        ( shape
                        , List.indexedMap (scanCell (nest + 1)) nestedCells
                        )
                Choice label state selected ( shape, nestedCells ) ->
                    Choice
                        label
                        state
                        selected
                        ( shape
                        , List.indexedMap (scanCell (nest + 1)) nestedCells
                        )
                newCell -> newCell

    in
        List.indexedMap (scanCell 0) cells


updateCell : ModelPos -> (Cell -> Cell) -> Model -> Model
updateCell ((ModelPos expectedNest expectedIndex) as modelPos) f model =
    traverseModel
        (\cell (ModelPos nest index) ->
            if (expectedNest == nest) && (expectedIndex == index) then
                f cell
            else cell)
        model


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch []


update : Msg -> Model -> Model -- ( UI, Cmd Msg )
update msg ui =
    case msg of
        Tune pos value ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Knob label _ -> Knob label value
                            _ -> cell
                    )
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
