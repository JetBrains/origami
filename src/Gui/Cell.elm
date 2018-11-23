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


textAttrs xPos yPos =
    [ fill "white"
    , x <| toString xPos, y <| toString yPos
    , fontSize "12"
    , S.style "font-family: sans-serif;"
    ]


renderCell : NestPos -> Maybe SelectionState -> Cell -> Html Msg
renderCell position isSelected cell =
    let cellBody =
            case cell of
                Knob label value ->
                    g []
                        [ text_ (textAttrs 35 35) [ Svg.text <| toString value ]
                        , circle [ cx "35", cy "35", r "20", stroke "aqua", fill "none" ] []
                        ]
                _ -> Svg.text "?"
        cellLabel =
            case cell of
                Knob label _ -> label
                Toggle label state -> label ++ " " ++ (if state == TurnedOn then "on" else "off")
                Button label _ -> label
                Nested label _ _ -> label ++ " ^"
                Choice label _ itemChosen _ -> label ++ " ^ (" ++ toString itemChosen ++ ")"
                ChoiceItem label -> label
    in svg []
        [ cellBody
        , text_
            (textAttrs 10 60)
            [ Svg.text <| cellLabel ]
        ]
