module Gui.Model exposing (..)


type ModelPos = ModelPos Int Int Int -- parent index + nest + index
--TODO: type ModelPos = ModelPos (List Int) -- just path by indices

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
    = Tune ModelPos Float
    | On ModelPos
    | Off ModelPos
    | ExpandNested ModelPos
    | CollapseNested ModelPos
    | ExpandChoice ModelPos
    | CollapseChoice ModelPos
    | Select ModelPos ModelPos
    | Move ModelPos Int
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


collapseAllAbove : ModelPos -> Nest -> Nest
collapseAllAbove  (ModelPos _ srcNest _) model =
    model |> traverseNest
        (\cell (ModelPos _ nest _) _ ->
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


traverseNest : (Cell -> ModelPos -> Maybe ModelPos -> Cell) -> Nest -> Nest
traverseNest f nest =
    { nest
    | cells = nest.cells |> traverseCells f
    }


traverseCells : (Cell -> ModelPos -> Maybe ModelPos -> Cell) -> Cells -> Cells
traverseCells f cells =
    let
        scanCell maybeParentPos parentIndex nestLevel index cell =
            let modelPos = (ModelPos parentIndex nestLevel index)
            in case f cell modelPos maybeParentPos of
                Nested label state nest ->
                    Nested
                        label
                        state
                        { nest
                        | cells =
                            nest.cells
                                |> List.indexedMap (scanCell (Just modelPos) index (nestLevel + 1))
                        }
                Choice label state selected nest ->
                    Choice
                        label
                        state
                        selected
                        { nest
                        | cells =
                            nest.cells |>
                                List.indexedMap (scanCell (Just modelPos) index (nestLevel + 1))
                        }
                newCell -> newCell

    in
        List.indexedMap (scanCell Nothing -1 0) cells


isSamePos : ModelPos -> ModelPos -> Bool
isSamePos (ModelPos lParent lNest lIndex) (ModelPos rParent rNest rIndex) =
    lParent == rParent && lNest == rNest && lIndex == rIndex


updateCell : ModelPos -> (Cell -> Cell) -> Nest -> Nest
updateCell expectedPos f nest =
    traverseNest
        (\cell modelPos _ ->
            if isSamePos modelPos expectedPos then
                f cell
            else cell)
        nest


