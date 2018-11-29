module Gui.Gui exposing
    ( Msg
    , Model
    , view
    , update
    )


import Gui.Cell exposing (..)
import Gui.Nest exposing (..)
import Gui.Grid exposing (..)


type alias Model umsg = Nest umsg
type alias View umsg = Grid umsg
type alias Msg umsg = Gui.Cell.Msg umsg
view = Gui.Grid.view


subscriptions : Model umsg -> Sub (Msg umsg)
subscriptions model = Sub.batch []


update
    : (umsg -> umodel -> ( umodel, Cmd umsg ))
    -> umodel
    -> Msg umsg
    -> Model umsg
    -> ( ( umodel, Cmd umsg ), Model umsg )
update userUpdate userModel msg ui =
    case msg of
        Tune pos value ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Knob label _ -> Knob label value
                            _ -> cell
                    )
            )
        On pos ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOn
                            _ -> cell
                    )
            )
        Off pos ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOff
                            _ -> cell
                    )
            )
        ExpandNested pos ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> collapseAllAbove pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Expanded cells
                            _ -> cell
                    )
            )
        CollapseNested pos ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Collapsed cells
                            _ -> cell

                  )
            )
        ExpandChoice pos ->
            ( userModel ! []
            , ui
                |> collapseAllAbove pos
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection handler cells ->
                                Choice label Expanded selection handler cells
                            _ -> cell
                    )
            )
        CollapseChoice pos ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection handler cells ->
                                Choice label Collapsed selection handler cells
                            _ -> cell
                    )
            )
        Select pos ->
            ( userModel ! []
            , let
                parentPos = getParentPos pos |> Maybe.withDefault nowhere
                index = getIndexOf pos |> Maybe.withDefault -1
            in
                ui
                    |> shiftFocusTo pos
                    |> updateCell parentPos
                        (\cell ->
                            case cell of
                                Choice label expanded selection handler cells ->
                                    Choice label expanded index handler cells
                                _ -> cell
                        )
            )

        SendToUser userMsg -> ( userUpdate userMsg userModel, ui )

        SelectAndSendToUser pos _ ->
            update userUpdate userModel (Select pos) ui

        ShiftFocusLeftAt pos ->
            ( userModel ! [], ui |> shiftFocusBy -1 pos )

        ShiftFocusRightAt pos ->
            ( userModel ! [], ui |> shiftFocusBy 1 pos )

        NoOp -> ( userModel ! [], ui )
