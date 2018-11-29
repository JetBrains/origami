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


update : Msg umsg -> Model umsg -> Model umsg -- ( UI, Cmd Msg )
update msg ui =
    case msg of
        Tune pos value ->
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Knob label _ -> Knob label value
                            _ -> cell
                    )
        On pos ->
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOn
                            _ -> cell
                    )
        Off pos ->
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOff
                            _ -> cell
                    )
        ExpandNested pos ->
            ui
                |> shiftFocusTo pos
                |> collapseAllAbove pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Expanded cells
                            _ -> cell
                    )
        CollapseNested pos ->
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Collapsed cells
                            _ -> cell
                    )
        ExpandChoice pos ->
            ui
                |> collapseAllAbove pos
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection cells ->
                                Choice label Expanded selection cells
                            _ -> cell
                    )
        CollapseChoice pos ->
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection cells ->
                                Choice label Collapsed selection cells
                            _ -> cell
                    )
        Select pos ->
            let
                parentPos = getParentPos pos |> Maybe.withDefault nowhere
                index = getIndexOf pos |> Maybe.withDefault -1
            in
                ui
                    |> shiftFocusTo pos
                    |> updateCell parentPos
                        (\cell ->
                            case cell of
                                Choice label expanded selection cells ->
                                    Choice label expanded index cells
                                _ -> cell
                        )

        ShiftFocusLeftAt pos ->
            ui |> shiftFocusBy -1 pos

        ShiftFocusRightAt pos ->
            ui |> shiftFocusBy 1 pos

        _ -> ui
