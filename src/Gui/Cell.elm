module Gui.Cell exposing (..)

import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

type alias Label = String

type alias ItemChosen = Int

type NestPos = NestPos (List Int) -- just path by indices

type alias Shape = ( Int, Int )

type alias Cells = List Cell

type alias Handler = (() -> ())
-- TODO: type alias Handler a = (() -> a)

type alias Nest =
    { focus: Int
    , shape: Shape
    , cells: Cells
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


type Cell
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label Handler
    | Nested Label ExpandState Nest
    | Choice Label ExpandState ItemChosen Nest
    | ChoiceItem Label


type Msg
    = NoOp
    | Tune NestPos Float
    | On NestPos
    | Off NestPos
    | ExpandNested NestPos
    | CollapseNested NestPos
    | ExpandChoice NestPos
    | CollapseChoice NestPos
    | Select NestPos
    -- | Move NestPos Int
    | ShiftFocusLeftAt NestPos
    | ShiftFocusRightAt NestPos
    -- | Color


renderCell : NestPos -> Maybe SelectionState -> Cell -> Html Msg
renderCell position isSelected cell =
    let cellBody =
            case cell of
                Knob label value ->
                    circle [ cx "50", cy "50", r "15", stroke "aqua", fill "none" ] []
                _ -> Svg.text "?"
        cellLabel =
            case cell of
                Knob label _ -> label
                _ -> "?"
    in svg []
        [ cellBody
        , text_
            [ fill "white", x "10", y "60", fontSize "12", S.style "font-family: sans-serif;" ]
            [ Svg.text <| cellLabel ]
        ]
