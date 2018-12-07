module Gui.Cell exposing (..)

import Array
import Html exposing (Html, text, div, span, input)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

import Gui.Def exposing (..)
import Gui.Msg exposing (..)


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
                Ghost _ ->
                    g [ class "gui-ghost" ]
                        [ ]
                Knob _ { min, max, step, roundBy } value _ ->
                    let
                        friendlyValue =
                            toFloat (round (value * toFloat roundBy)) / toFloat roundBy
                        normalizedValue = value / (max - min)
                        rotationAngle = 360 * normalizedValue
                    in
                        g [ class "gui-knob" ]
                            [ text_
                                (textAttrs (cellWidth / 2) (cellHeight / 3) baseColor)
                                [ Svg.text <| toString friendlyValue ]
                            , g
                                [ transform
                                    <| "translate(" ++ toString (cellWidth / 2) ++ ","
                                    ++ toString (cellHeight / 3) ++ ")"
                                ]
                                [ circle
                                    (circleAttrs 0 0 baseColor)
                                    []
                                , rect
                                    [ width "5"
                                    , height "5"
                                    , x <| toString (-2.5)
                                    , y <| toString (-2.5)
                                    , fill baseColor
                                    , stroke "none"
                                    , transform
                                        <| "rotate(" ++ toString rotationAngle ++ ") translate(0,-15)"
                                    ]
                                    []
                                ]

                            ]
                Toggle _ state _ ->
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
                Ghost label -> label
                Knob label _ _ _ -> label
                Toggle label _ _ -> label
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
