module Gui.View exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H

import Gui.Model exposing (..)


type GridPos = GridPos Int Int

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


emptyGrid : Shape -> Grid
emptyGrid (( width, height ) as shape)
    = Grid shape <| Array.repeat height (Array.repeat width Nothing)


bottomLeft : GridPos
bottomLeft = (GridPos 0 0)


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
    put -1 nest gridPos shape Nothing Nothing cells grid


put
    :  Int
    -> Int
    -> GridPos
    -> Shape
    -> Maybe ItemChosen
    -> Maybe ModelPos
    -> List Cell
    -> Grid
    -> Grid
put
    parentIndex
    nest
    (GridPos row col)
    currentShape
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
                    , modelPos = ModelPos parentIndex nest cellIndex
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
                Just { cell, modelPos } ->
                    let (ModelPos _ cellNest cellIndex) = modelPos
                    in if (cellNest == nest) then
                        case cell of
                            Nested _ Expanded ( shape, cells ) ->
                                put
                                    cellIndex
                                    (nest + 1)
                                    (findNextPos row col currentShape shape)
                                    shape
                                    Nothing
                                    (Just modelPos)
                                    cells
                                    grid
                            Choice _ Expanded selectedItem ( shape, cells ) ->
                                put
                                    cellIndex
                                    (nest + 1)
                                    (findNextPos row col currentShape shape)
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


showGridPos : GridPos -> String
showGridPos (GridPos row col) =
    "(" ++ toString row ++ "," ++ toString col ++ ")"


showModelPos : ModelPos -> String
showModelPos (ModelPos parent nest index) =
    "<" ++ toString parent ++ "," ++ toString nest ++ "," ++ toString index ++ ">"


view : Model -> Html Msg
view model =
    div [ H.class "gui" ]
        [ layout model |> viewGrid ]
