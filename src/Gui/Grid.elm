module Gui.Grid exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Json.Decode as Json


import Gui.Nest exposing (..)
import Gui.Cell exposing (..)


type GridPos = GridPos Int Int

type alias GridCell =
    { cell: Cell
    , nestPos: NestPos
    , isSelected: Maybe SelectionState -- if it's under Choice item, then it has selection state
    , isFocused: FocusState
    }

type alias Row = Array (Maybe GridCell)

type alias Rows = Array Row

type Grid = Grid Shape Rows


type Mode
    = DebugInfo
    | Fancy


type FocusState
    = Focused Int -- nest level
    | NotFocused


mode : Mode
mode = DebugInfo


emptyGrid : Shape -> Grid
emptyGrid (( width, height ) as shape)
    = Grid shape <| Array.repeat height (Array.repeat width Nothing)


bottomLeft : GridPos
bottomLeft = (GridPos 0 0)


doCellPurpose : GridCell -> Maybe Msg
doCellPurpose { cell, nestPos, isSelected } =
    case cell of
        Toggle _ val ->
            Just <| if val == TurnedOn then Off nestPos else On nestPos
        Nested _ state _ ->
            Just <| if state == Expanded then CollapseNested nestPos else ExpandNested nestPos
        Choice _ state _ _ ->
            Just <| if state == Expanded then CollapseChoice nestPos else ExpandChoice nestPos
        _ -> case isSelected of
            -- ( Just parentPos, Just Selected ) -> Deselect parentPos nestPos |> Just
            Just NotSelected -> Just <| Select nestPos
            _ -> Nothing


findHoverMessage : GridCell -> Maybe Msg
findHoverMessage { cell, nestPos }  =
    case cell of
        Knob label value ->
            Tune nestPos (value + 1) |> Just
        _ -> Nothing


findClickMessage : GridCell -> Maybe Msg
findClickMessage = doCellPurpose


-- findKeydownMessage : GridCell -> Int -> Msg
-- findKeydownMessage ({ cell, nestPos, isSelected } as gridCell) keyCode =
--     case Debug.log "keyCode" keyCode of
--         -- left arrow
--         37 -> ShiftFocusLeftAt nestPos
--         -- right arrow
--         39 -> ShiftFocusRightAt nestPos
--         -- space
--         33 -> doCellPurpose gridCell |> Maybe.withDefault NoOp
--         -- enter
--         13 -> doCellPurpose gridCell |> Maybe.withDefault NoOp
--         -- else
--         _ -> NoOp


viewCellContentDebug : GridPos -> GridCell -> Html Msg
viewCellContentDebug ((GridPos row col) as gridPos) { cell, nestPos, isSelected } =
    let
        posStr = showGridPos gridPos ++ " " ++ showNestPos nestPos
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


viewCellContent : GridPos -> GridCell -> Html Msg
viewCellContent gridPos gridCell =
    case mode of
        DebugInfo -> viewCellContentDebug gridPos gridCell
        Fancy ->
            case gridCell of
                { cell, nestPos, isSelected }
                    -> renderCell nestPos isSelected cell


viewCell : NestPos -> GridPos -> Maybe GridCell -> Html Msg
viewCell focus gridPos maybeGridCell =
    let
        findFocusIntensity cellNestLevel focusNestLevel =
            focusNestLevel - cellNestLevel
        getFocusIntensityClass cellNestLevel focus =
            "focused--" ++ toString
                (findFocusIntensity cellNestLevel <| getNestLevel focus)
        className =
            case maybeGridCell of
                Just { isSelected, isFocused } ->
                    case ( isSelected, isFocused ) of
                        ( Just Selected, Focused nestLevel ) ->
                            "cell selected focused " ++
                                getFocusIntensityClass nestLevel focus
                        ( Just Selected, NotFocused ) -> "cell selected"
                        ( Just NotSelected, Focused nestLevel ) ->
                            "cell focused " ++
                                getFocusIntensityClass nestLevel focus
                        ( Nothing, Focused nestLevel ) ->
                            "cell focused " ++
                                getFocusIntensityClass nestLevel focus
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
                        -- ) ++
                        -- (findKeydownMessage gridCell
                        --     |> (\tagger ->
                        --         [ H.on "keyup" (Json.map tagger H.keyCode)
                        --         ]
                        --       )
                        -- )
                    )
                |> Maybe.withDefault []
        attributes = [ H.class className ] ++ handlers
        children = maybeGridCell
            |> Maybe.map (\cell -> [ viewCellContent gridPos cell ])
            |> Maybe.withDefault []
    in
        div attributes children


viewRow : NestPos -> GridPos -> Row -> Html Msg
viewRow focus (GridPos row col) cols =
    Array.indexedMap
        (\subCol -> viewCell focus (GridPos row (col + subCol)))
        cols
        |> Array.toList
        |> div [ H.class "row" ]


viewRows : NestPos -> Rows -> Html Msg
viewRows focus rows =
    let
        origin  = bottomLeft
        (GridPos row col) = origin
        topRows =
            rows
                |> Array.indexedMap
                    (\subRow -> viewRow focus (GridPos (row + subRow) col))
                |> Array.toList
    in
        topRows |> div [ H.class "cells" ]



viewGrid : NestPos -> Grid -> Html Msg
viewGrid focus (Grid _ grid) =
    div [ H.class "grid" ]
        [ grid |> viewRows focus ]



putAtRoot : GridPos -> Nest -> Grid -> Grid
putAtRoot gridPos nest grid =
    put gridPos Nothing Nothing nest grid


put
    :  GridPos
    -> Maybe ItemChosen
    -> Maybe NestPos
    -> Nest
    -> Grid
    -> Grid
put
    (GridPos row col)
    maybeChosenItem
    maybeParent
    nest
    (Grid gridShape rows) =
    let
        --a = Debug.log "gPos" (GridPos row col)
        ( gridWidth, _ ) = gridShape
        parentNestLevel = maybeParent
            |> Maybe.map getNestLevel
            |> Maybe.withDefault 0
        currentShape = nest.shape
        cellsList = nest.cells
        cells = Array.fromList cellsList
            |> Array.indexedMap
                (\cellIndex cell ->
                    let nestPos = maybeParent |> deeperOrRoot cellIndex
                    in  { cell = cell
                        , nestPos = nestPos
                        , isSelected = case maybeChosenItem of
                            Just chosenIndex ->
                                Just <|
                                    if cellIndex == chosenIndex
                                    then Selected else NotSelected
                            _ -> Nothing
                        , isFocused = if nest.focus == cellIndex
                            then Focused <| getNestLevel nestPos
                            else NotFocused
                        }
                )
        -- hasNesting = Debug.log "nests" <| Array.map (\(_, (NestPos nest _)) -> nest) cells
        fits ( row, col ) ( width, height ) =
            (row < height) && ( col < width )
        indexOf ( row, col ) ( width, _ ) =
            row * width + col
        updateCell row_ col_ prevCell =
            if (row_ >= row) && (col_ >= col) then
                let
                    localPos = (row_ - row, col_ - col)
                in
                    if fits localPos currentShape then
                        case Array.get (indexOf localPos currentShape) cells of
                            Just newCell -> Just newCell
                            Nothing -> prevCell
                    else prevCell
            else prevCell
        updateRow row_ row =
            row |> Array.indexedMap (updateCell row_)
        findNextPos row_ col_ ( curWidth, curHeight ) ( nestedWidth, nestedHeight ) =
            if (col_ + nestedWidth < gridWidth) then
                GridPos (row_ + curHeight) col_
            else GridPos (row_ + curHeight) (gridWidth - nestedWidth)
        applyColExpands maybeCell ( col, grid ) =
            ( col + 1
            , case maybeCell of
                Just { cell, nestPos } ->
                    let ( cellNestLevel, cellIndex ) =
                        ( getNestLevel nestPos
                        , getIndexOf nestPos |> Maybe.withDefault -1
                        )
                    in if (cellNestLevel == parentNestLevel + 1) then
                        case cell of
                            Nested _ Expanded ({ shape } as nest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    Nothing
                                    (Just nestPos)
                                    nest
                                    grid
                            Choice _ Expanded selectedItem ({ shape } as nest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    (Just selectedItem)
                                    (Just nestPos)
                                    nest
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


layout : Nest -> Grid
layout nest =
    emptyGrid (10, 6)
        |> putAtRoot (GridPos 0 0) nest
        |> flip


flip : Grid -> Grid
flip (Grid shape rows) =
    rows
        |> Array.toList
        |> List.reverse
        |> Array.fromList
        |> Grid shape


showGridPos : GridPos -> String
showGridPos (GridPos row col) =
    "(" ++ toString row ++ "," ++ toString col ++ ")"


showNestPos : NestPos -> String
showNestPos (NestPos path) =
    "<" ++ (path |> List.reverse |> List.map toString |> String.join ",") ++ ">"


findGridCell : NestPos -> Grid -> Maybe GridCell
findGridCell searchFor (Grid _ rows) =
    rows |> Array.foldl
        (\row foundCell ->
            row |> Array.foldl
                (\maybeGridCell foundCell ->
                    case ( foundCell, maybeGridCell ) of
                        ( Nothing, Just ({ nestPos } as gridCell) ) ->
                            if (isSamePos searchFor nestPos) then
                                Just gridCell
                            else Nothing
                        _ -> foundCell
                ) foundCell
        ) Nothing


keyDownHandler : Nest -> Grid -> Int -> Msg
keyDownHandler nest grid keyCode =
    let
        currentFocus = findFocus nest
        currentFocus_ = case currentFocus of
            (NestPos path) -> Debug.log "currentFocus" path
        maybeCurrentCell = Debug.log "currentCell" <| findGridCell currentFocus grid
        executeCell = maybeCurrentCell
            |> Maybe.andThen doCellPurpose
            |> Maybe.withDefault NoOp
    -- Find top focus, with it either doCellPurpose or ShiftFocusRight/ShiftFocusLeft
    in
        case Debug.log "keyCode" keyCode of
            -- left arrow
            37 -> ShiftFocusLeftAt currentFocus
            -- right arrow
            39 -> ShiftFocusRightAt currentFocus
            -- up arrow
            -- 38 -> ShiftFocusUpAt currentFocus
            -- down arrow
            -- 40 -> ShiftFocusDownAt currentFocus
            -- up arrow
            38 -> maybeCurrentCell
                |> Maybe.map (\{ cell } ->
                        case cell of
                            Nested _ Collapsed _ -> ExpandNested currentFocus
                            Choice _ Collapsed _ _ -> ExpandChoice currentFocus
                            _ -> NoOp
                    )
                |> Maybe.withDefault NoOp -- execute as well?
            -- down arrow
            40 -> let parentFocus = currentFocus |> shallower in
                if (isSamePos parentFocus nowhere)
                    then NoOp
                    else
                        findGridCell parentFocus grid
                            |> Maybe.map (\{ cell } ->
                                    case cell of
                                        Nested _ Expanded _ -> CollapseNested parentFocus
                                        Choice _ Expanded _ _ -> CollapseChoice parentFocus
                                        _ -> NoOp
                                )
                            |> Maybe.withDefault NoOp
            -- space
            33 -> executeCell
            -- enter
            13 -> executeCell
            -- else
            _ -> NoOp


view : Nest -> Html Msg
view nest =
    let
        grid = layout nest
        focus = findFocus nest
    in
        div [ H.id "grid-gui"
            , H.class "gui"
            , H.tabindex -1
            , H.on "keydown"
                <| Json.map (keyDownHandler nest grid) H.keyCode
            ]
            [ grid |> viewGrid focus ]
