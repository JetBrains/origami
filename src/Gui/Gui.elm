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
type ModelPos = ModelPos Int Int -- nest + index
type alias Shape = ( Int, Int )

type alias Cells = List Cell
type alias Model = ( Shape, Cells )

type alias GridCell =
    { cell: Cell
    , modelPos: ModelPos
    , parentPos: Maybe ModelPos -- if it has parent, then there is its position
    , isSelected: Maybe SelectionState -- if it's under Choice item, then it has selection state
    }
type alias Row = Array (Maybe GridCell)
type alias Rows = Array Row
type Grid = Grid Shape Rows
type alias View = Grid
--type alias Path = CellPos


type alias Handler = (() -> ())
type alias Label = String
type alias ItemChosen = Int


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


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState (Shape, Cells)
    | Choice Label ExpandState ItemChosen (Shape, Cells)
    | ChoiceItem Label
    -- | Color


type Msg
    = Tune ModelPos Float
    | On ModelPos
    | Off ModelPos
    | ExpandNested ModelPos
    | CollapseNested ModelPos
    | ExpandChoice ModelPos
    | CollapseChoice ModelPos
    | Select ModelPos ModelPos
    | Move ModelPos Int


-- TODO:
-- initialMode : UiMode
-- initialMode = Development



emptyGrid : Shape -> Grid
emptyGrid (( width, height ) as shape)
    = Grid shape <| Array.repeat height (Array.repeat width Nothing)


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
        svgBlendGrid =
            ( ( 3, 3 )
            ,
                [ ChoiceItem "normal"
                , ChoiceItem "overlay"
                , ChoiceItem "multiply"
                , ChoiceItem "darken"
                , ChoiceItem "lighten"
                , ChoiceItem "multiply"
                , ChoiceItem "multiply"
                , ChoiceItem "multiply"
                , ChoiceItem "multiply"
                ]
            )

        amplitudeGrid = noChildren
        fssControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOff
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
                , Choice "blend" Collapsed 0 svgBlendGrid
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


showGridPos : GridPos -> String
showGridPos (GridPos row col) =
    "(" ++ toString row ++ "," ++ toString col ++ ")"


showModelPos : ModelPos -> String
showModelPos (ModelPos nest index) =
    "<" ++ toString nest ++ "," ++ toString index ++ ">"


findHoverMessage : GridCell -> Maybe Msg
findHoverMessage { cell, modelPos }  =
    case cell of
        Knob label value ->
            Tune modelPos (value + 1) |> Just
        _ -> Nothing


findClickMessage : GridCell -> Maybe Msg
findClickMessage { cell, modelPos, parentPos, isSelected } =
    case cell of
        Toggle _ val ->
            Just <| if val == TurnedOn then Off modelPos else On modelPos
        Nested _ state _ ->
            Just <| if state == Expanded then CollapseNested modelPos else ExpandNested modelPos
        Choice _ state _ _ ->
            Just <| if state == Expanded then CollapseChoice modelPos else ExpandChoice modelPos
        _ -> case ( parentPos, isSelected ) of
            -- ( Just parentPos, Just Selected ) -> Deselect parentPos modelPos |> Just
            ( Just parentPos, Just NotSelected ) -> Select parentPos modelPos |> Just
            _ -> Nothing


viewCell_ : GridPos -> GridCell -> Html Msg
viewCell_ ((GridPos row col) as gridPos) { cell, modelPos, isSelected } =
    let
        posStr = showGridPos gridPos ++ " " ++ showModelPos modelPos
    in case cell of
        Knob label val ->
            span []
                [ text <| posStr ++ " knob: " ++ label ++ " " ++ toString val ]
        Toggle label val ->
            span []
                [ text <| posStr ++ " toggle: " ++ label ++ " "
                    ++ (if val == TurnedOn then "on" else "off")
                ]
        Button label _ ->
            span []
                [ text <| posStr ++ " button: " ++ label ]
        Nested label state _ ->
            span []
                [ text <| posStr ++ " nested: " ++ label ++ " "
                    ++ (if state == Expanded then "expanded" else "collapsed")
                ]
        -- NestedItem level cell ->
        --     span [ ]
        --         [ text <| showPos pos ++ " nested item: " ++ toString level ++ " "
        --         , viewCell_ pos cell
        --         ]
        Choice label selected id _ ->
            span []
                [ text <| posStr ++ " choice: " ++ label ++ " "
                    ++ toString id
                ]
        ChoiceItem label ->
            span []
                [ text <| posStr ++ " choiceitem: " ++ label ++ " "
                    ++ (if isSelected == Just Selected then "selected" else "not-selected")
                ]



viewCell : GridPos -> Maybe GridCell -> Html Msg
viewCell gridPos maybeGridCell =
    let
        className =
            case maybeGridCell of
                Just { isSelected } ->
                    case isSelected of
                        Just Selected -> "cell selected"
                        _ -> "cell"
                _ -> "cell hole"
        handlers =
            maybeGridCell
                |> Maybe.map
                    (\gridCell ->
                        (findClickMessage gridCell
                            |> Maybe.map (\msg -> [ H.onClick msg ])
                            |> Maybe.withDefault []
                        ) ++
                        (findHoverMessage gridCell
                            |> Maybe.map (\msg -> [ H.onMouseOver msg ])
                            |> Maybe.withDefault []
                        )
                    )
                |> Maybe.withDefault []
        attributes = [ H.class className ] ++ handlers
        children = maybeGridCell
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
viewGrid (Grid _ grid) =
    div [ H.class "grid" ]
        [ viewRows grid ]



putAtRoot : Int -> GridPos -> Shape -> List Cell -> Grid -> Grid
putAtRoot nest gridPos shape cells grid =
    put nest gridPos shape Nothing Nothing cells grid


put : Int -> GridPos -> Shape -> Maybe ItemChosen -> Maybe ModelPos -> List Cell -> Grid -> Grid
put
    nest
    (GridPos row col)
    shape
    maybeChosenItem
    maybeParent
    cellsList
    (Grid gridShape rows) =
    let
        --a = Debug.log "gPos" (GridPos row col)
        ( gridWidth, _ ) = gridShape
        cells = Array.fromList cellsList
            |> Array.indexedMap
                (\cellIndex cell ->
                    { cell = cell
                    , modelPos = ModelPos nest cellIndex
                    , isSelected = case maybeChosenItem of
                        Just chosenIndex ->
                            Just <|
                                if cellIndex == chosenIndex
                                then Selected else NotSelected
                        _ -> Nothing
                    , parentPos = maybeParent
                    }
                )
        -- hasNesting = Debug.log "nests" <| Array.map (\(_, (ModelPos nest _)) -> nest) cells
        fits ( row, col ) ( width, height ) =
            (row < height) && ( col < width )
        indexOf ( row, col ) ( width, _ ) =
            row * width + col
        updateCell row_ col_ prevCell =
            if (row_ >= row) && (col_ >= col) then
                let
                    localPos = (row_ - row, col_ - col)
                in
                    if fits localPos shape then
                        case Array.get (indexOf localPos shape) cells of
                            Just newCell -> Just newCell
                            Nothing -> prevCell
                    else prevCell
            else prevCell
        updateRow row_ row =
            row |> Array.indexedMap (updateCell row_)
        findNextPos row_ col_ ( width, height ) =
            if (col_ + width < gridWidth) then
                GridPos (row_ + 1) col_
            else GridPos (row_ + 1) (gridWidth - width)
        applyColExpands maybeCell ( col, grid ) =
            ( col + 1
            , case maybeCell of
                Just { cell, modelPos } ->
                    let  (ModelPos cellNest _) = modelPos
                    in if (cellNest == nest) then
                        case cell of
                            Nested _ Expanded ( shape, cells ) ->
                                put
                                    (nest + 1)
                                    (findNextPos row col shape)
                                    shape
                                    Nothing
                                    (Just modelPos)
                                    cells
                                    grid
                            Choice _ Expanded selectedItem ( shape, cells ) ->
                                put
                                    (nest + 1)
                                    (findNextPos row col shape)
                                    shape
                                    (Just selectedItem)
                                    (Just modelPos)
                                    cells
                                    grid
                            _ -> grid
                    else grid
                _ -> grid
            )
        applyExpands row grid =
            Array.foldl applyColExpands ( 0, grid ) row
                |> Tuple.second
    in
        rows
            |> Array.indexedMap updateRow
            |> (\rows ->
                    Array.foldl applyExpands (Grid gridShape rows) rows
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


set : GridPos -> GridCell -> Grid -> Grid
set (GridPos row col) cell ((Grid shape rows) as grid) =
    Array.get row rows
        |> Maybe.map
            (\prevRow ->
                Array.set col (Just cell) prevRow
            )
        |> Maybe.map
            (\newRow ->
                Array.set row newRow rows)
        |> Maybe.map (Grid shape)
        |> Maybe.withDefault grid


layout : Model -> Grid
layout ( shape, cells ) =
    emptyGrid (10, 6)
        |> putAtRoot 0 (GridPos 0 0) shape cells
        |> flip


flip : Grid -> Grid
flip (Grid shape rows) =
    rows
        |> Array.toList
        |> List.reverse
        |> Array.fromList
        |> Grid shape


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


collapseAllAbove : ModelPos -> Model -> Model
collapseAllAbove  (ModelPos srcNest _) model =
    model |> traverseModel
        (\cell (ModelPos nest index) _ ->
            if (nest >= srcNest) then
                case cell of
                    Nested label _ nestedCells ->
                        Nested
                            label
                            Collapsed
                            nestedCells
                    Choice label _ selected nestedCells ->
                        Choice
                            label
                            Collapsed
                            selected
                            nestedCells
                    _ -> cell
            else cell
        )


traverseModel : (Cell -> ModelPos -> Maybe ModelPos -> Cell) -> Model -> Model
traverseModel f ( shape, cells ) =
    ( shape, traverseCells f cells )


traverseCells : (Cell -> ModelPos -> Maybe ModelPos -> Cell) -> Cells -> Cells
traverseCells f cells =
    let
        scanCell maybeParentPos nest index cell =
            let modelPos = (ModelPos nest index)
            in case f cell modelPos maybeParentPos of
                Nested label state ( shape, nestedCells ) ->
                    Nested
                        label
                        state
                        ( shape
                        , List.indexedMap (scanCell (Just modelPos) (nest + 1)) nestedCells
                        )
                Choice label state selected ( shape, nestedCells ) ->
                    Choice
                        label
                        state
                        selected
                        ( shape
                        , List.indexedMap (scanCell (Just modelPos) (nest + 1)) nestedCells
                        )
                newCell -> newCell

    in
        List.indexedMap (scanCell Nothing 0) cells


updateCell : ModelPos -> (Cell -> Cell) -> Model -> Model
updateCell ((ModelPos expectedNest expectedIndex) as modelPos) f model =
    traverseModel
        (\cell (ModelPos nest index) _ ->
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
        ExpandNested pos ->
            ui
                |> collapseAllAbove pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Expanded cells
                            _ -> cell
                    )
        CollapseNested pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Collapsed cells
                            _ -> cell
                    )
        ExpandChoice pos ->
            ui
                |> collapseAllAbove pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection cells ->
                                Choice label Expanded selection cells
                            _ -> cell
                    )
        CollapseChoice pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection cells ->
                                Choice label Collapsed selection cells
                            _ -> cell
                    )
        Select parentPos ((ModelPos  _ index) as pos) ->
            ui
                |> updateCell parentPos
                    (\cell ->
                        case cell of
                            Choice label expanded selection cells ->
                                Choice label expanded index cells
                            _ -> cell
                    )
        _ -> ui
