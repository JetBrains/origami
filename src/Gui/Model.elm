module Gui.Model exposing (..)


type ModelPos = ModelPos (List Int) -- just path by indices

type alias Shape = ( Int, Int )

type alias Cells = List Cell

type alias Handler = (() -> ())

type alias Label = String

type alias ItemChosen = Int

type alias Nest =
    { focus: Int
    , shape: Shape
    , cells: Cells
    }

type alias Model = Nest


type Msg
    = NoOp
    | Tune ModelPos Float
    | On ModelPos
    | Off ModelPos
    | ExpandNested ModelPos
    | CollapseNested ModelPos
    | ExpandChoice ModelPos
    | CollapseChoice ModelPos
    | Select ModelPos
    -- | Move ModelPos Int
    | ShiftFocusLeftAt ModelPos
    | ShiftFocusRightAt ModelPos
    -- | Color


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


type FocusState
    = Focused
    | NotFocused


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState Nest
    | Choice Label ExpandState ItemChosen Nest
    | ChoiceItem Label


noChildren : Nest
noChildren =
    { focus = -1
    , shape = ( 0, 0 )
    , cells = []
    }


oneLine : Cells -> Nest
oneLine cells =
    { focus = 0
    , shape = ( List.length cells, 1 )
    , cells = cells
    }


nest : Shape -> Cells -> Nest
nest shape cells =
    { focus = 0
    , shape = shape
    , cells = cells
    }


traverseNest : (Cell -> ModelPos -> Cell) -> Nest -> Nest
traverseNest f nest =
    { nest
    | cells = nest.cells |> traverseCells f
    }


traverseCells : (Cell -> ModelPos -> Cell) -> Cells -> Cells
traverseCells f cells =
    let
        scanCell maybeParentPos index cell =
            let modelPos =
                case  maybeParentPos of
                    Just parentPos -> parentPos |> deeper index
                    Nothing -> root index
            in case f cell modelPos of
                Nested label state nest ->
                    Nested
                        label
                        state
                        { nest
                        | cells =
                            nest.cells
                                |> List.indexedMap (scanCell (Just modelPos))
                        }
                Choice label state selected nest ->
                    Choice
                        label
                        state
                        selected
                        { nest
                        | cells =
                            nest.cells |>
                                List.indexedMap (scanCell (Just modelPos))
                        }
                newCell -> newCell

    in
        List.indexedMap (scanCell Nothing) cells


traverseAllNests : (Nest -> ModelPos -> Nest) -> Nest -> Nest
traverseAllNests f nest =
    { nest
    | cells = f nest nowhere |> .cells |> traverseCells
        (\cell cellPosition ->
            case cell of
                Nested label state nest ->
                    f nest cellPosition |> Nested label state
                Choice label state selected nest ->
                    f nest cellPosition |> Choice label state selected
                _ -> cell
        )
    }


foldCells : (Cell -> ModelPos -> a -> a) -> a -> Nest -> a
foldCells = foldCells_ Nothing


foldCells_ : Maybe ModelPos -> (Cell -> ModelPos -> a -> a) -> a -> Nest -> a
foldCells_ maybeParentPos f default { cells } =
    let
        foldingF maybeParentPos cell ( index, v ) =
            ( index + 1
            ,   let
                    modelPos =
                        case maybeParentPos of
                            Just parentPos -> parentPos |> deeper index
                            Nothing -> root index
                in case cell of
                    Nested _ _ nest ->
                        foldCells_ (Just modelPos) f (f cell modelPos v) nest
                        -- f cell modelPos <| foldCells_ (Just modelPos) f v nest
                    Choice _ _ _ nest ->
                        foldCells_ (Just modelPos) f (f cell modelPos v) nest
                        -- f cell modelPos <| foldCells_ (Just modelPos) f v nest
                    _ -> f cell modelPos v
            )
    in
        List.foldl (foldingF maybeParentPos) (0, default) cells
            |> Tuple.second


foldNests : (Nest -> ModelPos -> a -> a) -> a -> Nest -> a
foldNests f default nest =
    nest |>
        foldCells (\cell modelPos v ->
            case cell of
                Nested _ _ nest ->
                    f nest modelPos v
                Choice _ _ _ nest ->
                    f nest modelPos v
                _ -> v
        ) (f nest nowhere default)


nowhere : ModelPos
nowhere = ModelPos []


root : Int -> ModelPos
root index =
    ModelPos [ index ]


deeper : Int -> ModelPos -> ModelPos
deeper index (ModelPos path) =
    ModelPos (index :: path)


deeperOrRoot : Int -> Maybe ModelPos -> ModelPos
deeperOrRoot index maybePos =
    maybePos
        |> Maybe.map (deeper index)
        |> Maybe.withDefault (root index)


getNestLevel : ModelPos -> Int
getNestLevel (ModelPos path) =
    List.length path


getIndexOf : ModelPos -> Maybe Int
getIndexOf (ModelPos path) =
    List.head path


getParentPos : ModelPos -> Maybe ModelPos
getParentPos (ModelPos path) =
    List.tail path
        |> Maybe.andThen
            (\parentPath ->
                case parentPath of
                    [] -> Nothing
                    _ -> Just parentPath
            )
        |> Maybe.map ModelPos


isSamePos : ModelPos -> ModelPos -> Bool
isSamePos (ModelPos lPath) (ModelPos rPath) = lPath == rPath


updateCell : ModelPos -> (Cell -> Cell) -> Nest -> Nest
updateCell expectedPos f nest =
    traverseNest
        (\cell modelPos ->
            if isSamePos modelPos expectedPos then
                f cell
            else cell)
        nest


collapseAllAbove : ModelPos -> Nest -> Nest
collapseAllAbove position nest =
    nest |> traverseNest
        (\cell cellPosition ->
            if (getNestLevel cellPosition >= getNestLevel position) then
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


shiftFocusTo : ModelPos -> Nest -> Nest
shiftFocusTo position nest =
    let
        maybeParentPos = getParentPos position
        focusOn = getIndexOf position
                    |> Maybe.withDefault -1
    in
        case maybeParentPos of
            Just parentPos ->
                nest |> traverseAllNests
                    (\deeperNest cellPosition ->
                        if (isSamePos cellPosition parentPos) then
                            { deeperNest
                            | focus = focusOn
                            }
                        else deeperNest
                    )
            Nothing ->
                { nest
                | focus = focusOn
                }


isDeeper : ModelPos -> ModelPos -> Bool
isDeeper (ModelPos lPath) (ModelPos rPath) =
    List.length lPath > List.length rPath


shiftFocusBy : Int -> ModelPos -> Nest -> Nest
shiftFocusBy amount position nest =
    let
        index = getIndexOf position |> Maybe.withDefault 0
        maybeParentPos = getParentPos position
        ensureFits { cells } =
            if ((index + amount) > 0) && (index + amount < List.length cells) then
                index + amount
            else index
    in
        case maybeParentPos of
            Just parentPos ->
                nest |> traverseAllNests
                    (\deeperNest cellPosition ->
                        if (isSamePos cellPosition parentPos) then
                            { deeperNest
                            | focus = ensureFits deeperNest
                            }
                        else deeperNest
                    )
            Nothing ->
                { nest
                | focus = ensureFits nest
                }


findFocus: Nest -> ModelPos
findFocus nest =
    nest |>
        foldNests (\{ focus } pos prevFocus ->
            let focusPos = pos |> deeper focus
            in if isDeeper focusPos prevFocus
                then focusPos
                else prevFocus
        ) nowhere
