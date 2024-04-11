module Gui.Grid exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Json.Decode as Json


import Gui.Def exposing (..)
import Gui.Msg exposing (..)
import Gui.Cell exposing (..)
import Gui.Nest exposing (..)


type alias GridCell umsg =
    { cell: Cell umsg
    , nestPos: NestPos
    -- if it's Choice Item, then we'll call this handler on click:
    , onSelect : Maybe (String -> umsg)
     -- if it's Choice item, then it has selection state:
    , isSelected: Maybe SelectionState
    , isFocused: FocusState
    }


type GridPos = GridPos Int Int
type alias Row umsg = Array (Maybe (GridCell umsg))
type alias Rows umsg = Array (Row umsg)
type Grid umsg = Grid Shape (Rows umsg)


type Mode
    = DebugInfo
    | Fancy


type FocusState
    = Focused Int -- nest level
    | NotFocused


mode : Mode
mode = Fancy


emptyGrid : Shape -> Grid umsg
emptyGrid (( width, height ) as shape)
    = Grid shape <| Array.repeat height (Array.repeat width Nothing)


bottomLeft : GridPos
bottomLeft = (GridPos 0 0)


doCellPurpose : GridCell umsg -> Msg umsg
doCellPurpose { cell, nestPos, isSelected, onSelect } =
    case cell of
        Knob _ _ _ _ ->
            FocusOn nestPos -- FIXME: NoOp? We do the same on mousedown
        Toggle _ val handler ->
            -- if val == TurnedOn then ToggleOff nestPos else ToggleOn nestPos
            if val == TurnedOn then
                handler TurnedOff |> ToggleOffAndSendToUser nestPos
            else
                handler TurnedOn |> ToggleOnAndSendToUser nestPos
        Nested _ state _ ->
            if state == Expanded then CollapseNested nestPos else ExpandNested nestPos
        Choice _ state _ _ _ ->
            if state == Expanded then CollapseChoice nestPos else ExpandChoice nestPos
        Button _ handler ->
            handler () |> SendToUser
        ChoiceItem label ->
            -- ( Just parentPos, Just Selected ) -> Deselect parentPos nestPos |> Just
            --case Debug.log "isSelected" isSelected of
            case isSelected of
                Just NotSelected ->
                    -- (Debug.log "onSelect" onSelect)
                    onSelect
                        |> Maybe.map ((|>) label)
                        |> Maybe.map (SelectAndSendToUser nestPos)
                        |> Maybe.withDefault (Select nestPos)
                _ -> NoOp
        _ ->
            case isSelected of
                Just NotSelected -> Select nestPos
                _ -> NoOp


maybeFocus : GridCell umsg -> Msg umsg
maybeFocus { cell, nestPos } =
    case cell of
        Knob _ _ _ _ ->
            FocusOn nestPos
        _ -> NoOp


viewCellContentDebug : GridPos -> GridCell umsg -> Html (Msg umsg)
viewCellContentDebug ((GridPos row col) as gridPos) { cell, nestPos, isSelected } =
    let
        posStr = showGridPos gridPos ++ " " ++ showNestPos nestPos
    in case cell of
        Ghost label  ->
            span []
                [ text <| posStr ++ " ghost: " ++ label ]
        Knob label { min, step, max } val _ ->
            span []
                [ text <| posStr ++ " knob: " ++ label
                    ++ " " ++ String.fromFloat min ++ "/"
                    ++ String.fromFloat step ++ "/"
                    ++ String.fromFloat max
                    ++ " " ++ String.fromFloat val ]
        Toggle label val _ ->
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
        Choice label selected id _ _ ->
            span []
                [ text <| posStr ++ " choice: " ++ label ++ " "
                    ++ String.fromInt id
                ]
        ChoiceItem label ->
            span []
                [ text <| posStr ++ " choiceitem: " ++ label ++ " "
                    ++ (if isSelected == Just Selected then "selected" else "not-selected")
                ]


viewCellContent : Focus -> GridPos -> GridCell umsg -> Html (Msg umsg)
viewCellContent focus gridPos gridCell =
    case mode of
        DebugInfo -> viewCellContentDebug gridPos gridCell
        Fancy ->
            case gridCell of
                { cell, nestPos, isSelected }
                    -> renderCell nestPos focus isSelected cell


viewCell : Focus -> GridPos -> Maybe (GridCell umsg) -> Html (Msg umsg)
viewCell focus gridPos maybeGridCell =
    let
        findFocusIntensity cellNestLevel focusNestLevel =
            focusNestLevel - cellNestLevel
        getFocusIntensityClass cellNestLevel (Focus innerFocus) =
            "focused--" ++ String.fromInt
                (findFocusIntensity cellNestLevel <| getNestLevel innerFocus)
        getLevelIntensityClass cellNestLevel (Focus innerFocus) =
            "level--" ++ String.fromInt
                (findFocusIntensity cellNestLevel <| getNestLevel innerFocus)
        className =
            case maybeGridCell of
                Just { isSelected, isFocused, nestPos } ->
                    (case ( isSelected, isFocused ) of
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
                        _ -> "cell")
                            ++ " " ++ getLevelIntensityClass
                                        (getNestLevel nestPos) focus
                _ -> "cell hole"
        handlers =
            maybeGridCell
                |> Maybe.map
                    (\gridCell ->
                        [ H.onClick <| doCellPurpose gridCell
                        , H.onMouseDown <| maybeFocus gridCell
                        ]
                    )
                |> Maybe.withDefault []
        attributes = [ H.class className ] ++ handlers
        children = maybeGridCell
            |> Maybe.map (\cell -> [ viewCellContent focus gridPos cell ])
            |> Maybe.withDefault []
    in
        div attributes children


viewRow : Focus -> GridPos -> Row umsg -> Html (Msg umsg)
viewRow focus (GridPos row col) cols =
    Array.indexedMap
        (\subCol -> viewCell focus (GridPos row (col + subCol)))
        cols
        |> Array.toList
        |> div [ H.class "row" ]


viewRows : Focus -> Rows umsg -> Html (Msg umsg)
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



viewGrid : Int -> Focus -> Grid umsg -> Html (Msg umsg)
viewGrid cellCount focus (Grid _ grid) =
    let
        width = (cellCount * (cellWidth + 2)) + (cellCount * cellMargin * 2)
    in
        div [ H.class "grid"
            , H.style "width" (String.fromInt width ++ "px")
            ]
            [ grid |> viewRows focus ]



putAtRoot : GridPos -> Nest umsg -> Grid umsg -> Grid umsg
putAtRoot gridPos nest grid =
    put gridPos Nothing Nothing Nothing nest grid


put
    :  GridPos
    -> Maybe ItemChosen
    -> Maybe NestPos
    -> Maybe (ChoiceHandler umsg)
    -> Nest umsg
    -> Grid umsg
    -> Grid umsg
put
    (GridPos row col)
    maybeChosenItem
    maybeParent
    maybeSelectHandler
    nest
    (Grid gridShape rows) =
    let
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
                        , onSelect = maybeSelectHandler -- we assume it is the choice item, if Just
                            |> Maybe.map ((|>) cellIndex)
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
        fits ( locRow, locCol ) ( width, height ) =
            (locRow < height) && ( locCol < width )
        indexOf ( locRow, locCol ) ( width, _ ) =
            locRow * width + locCol
        updateCell locRow locCol prevCell =
            if (locRow >= row) && (locCol >= col) then
                let
                    localPos = (locRow - row, locCol - col)
                in
                    if fits localPos currentShape then
                        case Array.get (indexOf localPos currentShape) cells of
                            Just newCell -> Just newCell
                            Nothing -> prevCell
                    else prevCell
            else prevCell
        updateRow rowIndex innerRow =
            innerRow |> Array.indexedMap (updateCell rowIndex)
        findNextPos row_ col_ ( curWidth, curHeight ) ( nestedWidth, nestedHeight ) =
            if (col_ + nestedWidth < gridWidth) then
                GridPos (row_ + curHeight) col_
            else GridPos (row_ + curHeight) (gridWidth - nestedWidth)
        applyColExpands maybeCell ( locCol, grid ) =
            ( locCol + 1
            , case maybeCell of
                Just { cell, nestPos } ->
                    let
                        ( cellNestLevel, cellIndex ) =
                            ( getNestLevel nestPos
                            , getIndexOf nestPos |> Maybe.withDefault -1
                            )
                    in if (cellNestLevel == parentNestLevel + 1) then
                        case cell of
                            Nested _ Expanded ({ shape } as innerNest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    Nothing
                                    (Just nestPos)
                                    Nothing
                                    innerNest
                                    grid
                            Choice _ Expanded selectedItem handler ({ shape } as innerNest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    (Just selectedItem)
                                    (Just nestPos)
                                    (Just handler)
                                    innerNest
                                    grid
                            _ -> grid
                    else grid
                _ -> grid
            )
        applyExpands locRow grid =
            Array.foldl applyColExpands ( 0, grid ) locRow
                |> Tuple.second
    in
        rows
            |> Array.indexedMap updateRow
            |> (\innerRows ->
                    Array.foldl applyExpands (Grid gridShape innerRows) innerRows
                )


set : GridPos -> GridCell umsg -> Grid umsg -> Grid umsg
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


layout : Nest umsg -> Grid umsg
layout nest =
    emptyGrid (10, 6)
        |> putAtRoot (GridPos 0 0) nest
        |> flip


flip : Grid umsg -> Grid umsg
flip (Grid shape rows) =
    rows
        |> Array.toList
        |> List.reverse
        |> Array.fromList
        |> Grid shape


showGridPos : GridPos -> String
showGridPos (GridPos row col) =
    "(" ++ String.fromInt row ++ "," ++ String.fromInt col ++ ")"


showNestPos : NestPos -> String
showNestPos (NestPos path) =
    "<" ++ (path |> List.reverse |> List.map String.fromInt |> String.join ",") ++ ">"


findGridCell : NestPos -> Grid umsg -> Maybe (GridCell umsg)
findGridCell searchFor (Grid _ rows) =
    rows |> Array.foldl
        (\row foundCell ->
            row |> Array.foldl
                (\maybeGridCell cellFoundInRow ->
                    case ( cellFoundInRow, maybeGridCell ) of
                        ( Nothing, Just ({ nestPos } as gridCell) ) ->
                            if (isSamePos searchFor nestPos) then
                                Just gridCell
                            else Nothing
                        _ -> cellFoundInRow
                ) foundCell
        ) Nothing


keyDownHandler : Nest umsg -> Grid umsg -> Int -> Msg umsg
keyDownHandler nest grid keyCode =
    let
        (Focus currentFocus) = findFocus nest
        maybeCurrentCell = findGridCell currentFocus grid
        executeCell = maybeCurrentCell
            |> Maybe.map doCellPurpose
            |> Maybe.withDefault NoOp
    -- Find top focus, with it either doCellPurpose or ShiftFocusRight/ShiftFocusLeft
    in
        case keyCode of
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
                            Choice _ Collapsed _ _ _ -> ExpandChoice currentFocus
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
                                        Choice _ Expanded _ _ _ -> CollapseChoice parentFocus
                                        _ -> NoOp
                                )
                            |> Maybe.withDefault NoOp
            -- space
            33 -> executeCell
            -- enter
            13 -> executeCell
            -- else
            _ -> NoOp


view : Nest umsg -> Html (Msg umsg)
view nest =
    let
        grid = layout nest
        focus = findFocus nest
        cellCount = sizeOf nest
        --keyDownHandlers = Json.map (\_ -> [ NoOp ]) H.keyCode
        keyDownHandler_ = H.on "keydown" <| Json.map (keyDownHandler nest grid) H.keyCode
    in
        div [ H.id "grid-gui"
            , H.class "gui noselect"
            , H.tabindex -1
            , keyDownHandler_
            ]
            [ grid |> viewGrid cellCount focus ]
