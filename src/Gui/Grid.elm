module Gui.Grid exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Json.Decode as Json


import Gui.Nest exposing (..)
import Gui.Cell exposing (..)


type GridPos = GridPos Int Int

type alias GridCell umsg =
    { cell: Cell umsg
    , nestPos: NestPos
    -- if it's Choice Item, then we'll call this handler on click:
    , onSelect : Maybe (String -> umsg)
     -- if it's Choice item, then it has selection state:
    , isSelected: Maybe SelectionState
    , isFocused: FocusState
    }

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
doCellPurpose { cell, nestPos, isSelected } =
    case cell of
        Toggle _ val ->
            if val == TurnedOn then Off nestPos else On nestPos
        Nested _ state _ ->
            if state == Expanded then CollapseNested nestPos else ExpandNested nestPos
        Choice _ state _ _ _ ->
            if state == Expanded then CollapseChoice nestPos else ExpandChoice nestPos
        Button _ handler ->
            handler () |> UserMsg
        _ -> case isSelected of
            -- ( Just parentPos, Just Selected ) -> Deselect parentPos nestPos |> Just
            Just NotSelected -> Select nestPos
            _ -> NoOp


findHoverMessage : GridCell umsg -> Msg umsg
findHoverMessage { cell, nestPos }  =
    case cell of
        Knob label value ->
            Tune nestPos (value + 1)
        _ -> NoOp


findClickMessage : GridCell umsg -> Msg umsg
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


viewCellContentDebug : GridPos -> GridCell umsg -> Html (Msg umsg)
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
        Choice label selected id _ _ ->
            span []
                [ text <| posStr ++ " choice: " ++ label ++ " "
                    ++ toString id
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
        getFocusIntensityClass cellNestLevel (Focus focus) =
            "focused--" ++ toString
                (findFocusIntensity cellNestLevel <| getNestLevel focus)
        getLevelIntensityClass cellNestLevel (Focus focus) =
            "level--" ++ toString
                (findFocusIntensity cellNestLevel <| getNestLevel focus)
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
                        [ H.onClick <| findClickMessage gridCell
                        , H.onMouseOver <| findHoverMessage gridCell
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



viewGrid : Focus -> Grid umsg -> Html (Msg umsg)
viewGrid focus (Grid _ grid) =
    div [ H.class "grid" ]
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
                        , onSelect = maybeSelectHandler
                            |> Maybe.map (\handler -> handler cellIndex)
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
                                    Nothing
                                    nest
                                    grid
                            Choice _ Expanded selectedItem handler ({ shape } as nest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    (Just selectedItem)
                                    (Just nestPos)
                                    (Just handler)
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
    "(" ++ toString row ++ "," ++ toString col ++ ")"


showNestPos : NestPos -> String
showNestPos (NestPos path) =
    "<" ++ (path |> List.reverse |> List.map toString |> String.join ",") ++ ">"


findGridCell : NestPos -> Grid umsg -> Maybe (GridCell umsg)
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


keyDownHandler : Nest umsg -> Grid umsg -> Int -> Msg umsg
keyDownHandler nest grid keyCode =
    let
        (Focus currentFocus) = findFocus nest
        maybeCurrentCell = Debug.log "currentCell" <| findGridCell currentFocus grid
        executeCell = maybeCurrentCell
            |> Maybe.map doCellPurpose
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
        --keyDownHandlers = Json.map (\_ -> [ NoOp ]) H.keyCode
        keyDownHandler_ = H.on "keydown" <| Json.map (keyDownHandler nest grid) H.keyCode
    in
        div [ H.id "grid-gui"
            , H.class "gui"
            , H.tabindex -1
            , keyDownHandler_
            ]
            [ grid |> viewGrid focus ]
