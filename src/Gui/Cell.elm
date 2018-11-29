module Gui.Cell exposing (..)

import Array
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

type alias Label = String

type alias ItemChosen = Int

type NestPos = NestPos (List Int) -- just path by indices

type Focus = Focus NestPos

type alias Shape = ( Int, Int )

type alias Cells umsg = List (Cell umsg)

type alias Handler umsg = (() -> umsg)

type alias ChoiceHandler umsg = (Int -> String -> () -> umsg)


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


type Cell umsg
    = Knob Label Float
    | Toggle Label ToggleState
    | Button Label (Handler umsg)
    | Nested Label ExpandState (Nest umsg)
    | Choice Label ExpandState ItemChosen (ChoiceHandler umsg) (Nest umsg)
    | ChoiceItem Label
    -- | ChoiceItem Int Label


type Msg umsg
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
    | SendToUser umsg
    | SelectAndSendToUser NestPos umsg
    -- | Color


cellWidth = 70
cellHeight = 70


labelColor = "white"
baseColor = "aqua"
onColor = "green"
offColor = "red"
nothingColor = "darkgray"
lineWidth = "2"


textAttrs : Float -> Float -> String -> List (Attribute (Msg umsg))
textAttrs xPos yPos color =
    [ fill color
    , x <| toString xPos, y <| toString yPos
    , fontSize "12"
    , S.style "font-family: sans-serif;"
    , textAnchor "middle"
    , alignmentBaseline "central"
    ]


circleAttrs : Float -> Float -> String -> List (Attribute (Msg umsg))
circleAttrs xPos yPos color =
    [ cx <| toString <| xPos
    , cy <| toString <| yPos
    , r "15"
    , stroke color, fill "none"
    , strokeWidth lineWidth
    ]


upArrow : Float -> Float -> String -> Svg (Msg umsg)
upArrow xPos yPos color =
    g
        [ class "gui-arrow"
        , transform "rotate(-180.000000) translate(-30, -12)" ]
        [
            Svg.path
                [ x <| toString <| xPos, y <| toString <| yPos
                --transform "translate(-37.125,-290.25)"
                , d "m 0 0 l 20 20 l 20 -20"
                , stroke color, fill "none"
                , strokeWidth lineWidth
                , S.strokeLinecap "round" ]
                []
        ]


downArrow : Float -> Float -> String -> Svg (Msg umsg)
downArrow xPos yPos color =
    g
        [ class "gui-arrow"
        , transform "rotate(-180.000000) translate(-30, -12)" ]
        [
            Svg.path
                [ x <| toString <| xPos, y <| toString <| yPos
                --transform "translate(-37.125,-290.25)"
                , d "m 0 20 l 20 -20 l 20 20"
                , stroke color, fill "none"
                , strokeWidth lineWidth
                , S.strokeLinecap "round" ]
                []
        ]



renderCell : NestPos -> Focus -> Maybe SelectionState -> Cell umsg -> Html (Msg umsg)
renderCell position (Focus focus) isSelected cell =
    let cellBody =
            case cell of
                Knob _ value ->
                    g [ class "gui-knob" ]
                        [ text_
                            (textAttrs (cellWidth / 2) (cellHeight / 3) baseColor)
                            [ Svg.text <| toString value ]
                        , circle
                            (circleAttrs (cellWidth / 2) (cellHeight / 3) baseColor)
                            []
                        ]
                Toggle _ state ->
                    g [ class "gui-toggle" ]
                        [ text_
                            (textAttrs (cellWidth / 2) (cellHeight / 3) baseColor)
                            [ Svg.text <| if state == TurnedOn then "on" else "off" ]
                        , circle
                            (circleAttrs (cellWidth / 2) (cellHeight / 3) <|
                                if state == TurnedOn then onColor else offColor)
                            []
                        ]
                Choice _ state itemChosen _ { cells } ->
                    g [ class "gui-choice" ]
                        [ text_
                            (textAttrs (cellWidth / 2) (cellHeight / 2) baseColor)
                            [ cells
                                |> Array.fromList |> Array.get itemChosen
                                |> Maybe.map cellLabel |> Maybe.withDefault "" |> Svg.text ]
                        , g
                            [ transform
                                <| "translate(" ++ (toString <| cellWidth / 2 - 10) ++ ",10)" ]
                            [ if state == Expanded
                                then downArrow (cellWidth / 2) 10 baseColor
                                else upArrow (cellWidth / 2) 10 baseColor ]
                        ]
                Nested _ state { cells } ->
                    g [ class "gui-nested" ]
                        [ g
                            [ transform
                                <| "translate(" ++ (toString <| cellWidth / 2 - 10) ++ ",10)" ]
                            [ if state == Expanded
                                then downArrow (cellWidth / 2) 10 baseColor
                                else upArrow (cellWidth / 2) 10 baseColor  ]
                        ]
                ChoiceItem _ ->
                    g [ class "gui-choice-item" ]
                        [ g
                            [ transform
                                <| "translate(" ++ (toString <| cellWidth / 2 - 10) ++ ",10)" ]
                            [ case isSelected of
                                (Just Selected) -> downArrow (cellWidth / 2) 10 onColor
                                _ -> downArrow (cellWidth / 2) 10 nothingColor ]
                        ]
                Button _ _ ->
                    g []
                        [ circle
                            (circleAttrs (cellWidth / 2) (cellHeight / 3) nothingColor)
                            []
                        ]
        cellLabel cell =
            case cell of
                Knob label _ -> label
                Toggle label state -> label
                Button label _ -> label
                Nested label _ _ -> label
                Choice label _ itemChosen _ _ -> label
                ChoiceItem label -> label
        labelBackWidth = cellWidth * 2/3
        labelBackHeight = 20
    in svg
        [ width <| toString cellWidth
        , height <| toString cellHeight
        ]
        [ cellBody
--        , rect
--            [ x <| toString <| (cellWidth / 2) - (labelBackWidth / 2)
--            , y <| toString <| cellHeight - labelBackHeight
--            , rx "5", ry "5"
--            , width <| toString <| labelBackWidth, height <| toString <| labelBackHeight
--            , fill "rgba(120,0,0,0.4)" ] []
        , text_
            (textAttrs (cellWidth / 2) (cellHeight - 10) "white")
            [ Svg.text <| cellLabel cell ]
        ]
