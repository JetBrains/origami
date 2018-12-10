module Gui.Def exposing (..)


import Gui.Mouse exposing (..)


type alias Label = String

type alias ItemChosen = Int

type NestPos = NestPos (List Int) -- just path by indices

type Focus = Focus NestPos

type alias Shape = ( Int, Int )

type alias Cells umsg = List (Cell umsg)

type alias Handler umsg = (() -> umsg)

type alias ChoiceHandler umsg = (Int -> String -> umsg)

type alias ToggleHandler umsg = (ToggleState -> umsg)

type alias KnobHandler umsg = (Float -> umsg)


type alias KnobState =
    { min : Float
    , max : Float
    , step : Float
    , roundBy : Int
    , default : Float
    }


type alias Nest umsg =
    { focus: Int
    , shape: Shape
    , cells: Cells umsg
    }


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


-- type AlterKnob
--     = Up
--     | Down
--     | Stay


type AlterKnob
    = Stay
    | Alter Float -- from -0.5 to 0.5


type Cell umsg
    = Ghost Label
    | Knob Label KnobState Float (KnobHandler umsg)
    | Toggle Label ToggleState (ToggleHandler umsg)
    | Button Label (Handler umsg)
    | Nested Label ExpandState (Nest umsg)
    | Choice Label ExpandState ItemChosen (ChoiceHandler umsg) (Nest umsg)
    | ChoiceItem Label
    -- | ChoiceItem Int Label


cellWidth = 70
cellHeight = 70
cellMargin = 5


knobDistance = cellHeight * 4


labelColor = "white"
baseColor = "aqua"
onColor = "green"
offColor = "red"
nothingColor = "darkgray"
lineWidth = "2"
