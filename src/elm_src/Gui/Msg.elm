module Gui.Msg exposing (..)


import Gui.Def exposing (..)
import Gui.Mouse exposing (..)


type Msg umsg
    = NoOp
    | TrackMouse MouseState
    | FocusOn NestPos
    | Tune NestPos AlterKnob
    | ToggleOn NestPos
    | ToggleOff NestPos
    | ExpandNested NestPos
    | CollapseNested NestPos
    | ExpandChoice NestPos
    | CollapseChoice NestPos
    | Select NestPos
    -- | Move NestPos Int
    | ShiftFocusLeftAt NestPos
    | ShiftFocusRightAt NestPos
    | SendToUser umsg
    | SelectAndSendToUser NestPos umsg
    | ToggleOnAndSendToUser NestPos umsg
    | ToggleOffAndSendToUser NestPos umsg
    | TuneAndApply NestPos AlterKnob umsg
    -- | Color
