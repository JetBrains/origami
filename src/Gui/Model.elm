module Gui.Model exposing (..)


type ModelPos = ModelPos Int Int Int -- parent index + nest + index
--TODO: type ModelPos = ModelPos (List Int) -- just path by indices

type alias Shape = ( Int, Int )

type alias Cells = List Cell

type alias Handler = (() -> ())

type alias Label = String

type alias ItemChosen = Int

type alias Model = ( Shape, Cells )


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


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState (Shape, Cells)
    | Choice Label ExpandState ItemChosen (Shape, Cells)
    | ChoiceItem Label


noChildren : ( Shape, Cells )
noChildren =
    ( ( 0, 0 ), [] )


oneLine : Cells -> ( Shape, Cells )
oneLine cells =
    ( ( List.length cells, 1 ), cells )


collapseAllAbove : ModelPos -> Model -> Model
collapseAllAbove  (ModelPos _ srcNest _) model =
    model |> traverseModel
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


traverseModel : (Cell -> ModelPos -> Maybe ModelPos -> Cell) -> Model -> Model
traverseModel f ( shape, cells ) =
    ( shape, traverseCells f cells )


traverseCells : (Cell -> ModelPos -> Maybe ModelPos -> Cell) -> Cells -> Cells
traverseCells f cells =
    let
        scanCell maybeParentPos parentIndex nest index cell =
            let modelPos = (ModelPos parentIndex nest index)
            in case f cell modelPos maybeParentPos of
                Nested label state ( shape, nestedCells ) ->
                    Nested
                        label
                        state
                        ( shape
                        , List.indexedMap (scanCell (Just modelPos) index (nest + 1)) nestedCells
                        )
                Choice label state selected ( shape, nestedCells ) ->
                    Choice
                        label
                        state
                        selected
                        ( shape
                        , List.indexedMap (scanCell (Just modelPos) index (nest + 1)) nestedCells
                        )
                newCell -> newCell

    in
        List.indexedMap (scanCell Nothing -1 0) cells


isSamePos : ModelPos -> ModelPos -> Bool
isSamePos (ModelPos lParent lNest lIndex) (ModelPos rParent rNest rIndex) =
    lParent == rParent && lNest == rNest && lIndex == rIndex


updateCell : ModelPos -> (Cell -> Cell) -> Model -> Model
updateCell expectedPos f model =
    traverseModel
        (\cell modelPos _ ->
            if isSamePos modelPos expectedPos then
                f cell
            else cell)
        model


