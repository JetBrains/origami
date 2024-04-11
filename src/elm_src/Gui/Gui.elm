module Gui.Gui exposing
    ( Msg, Model
    , view, update, build
    , moves, ups, downs
    , extractMouse
    )


import Gui.Def exposing (..)
import Gui.Msg exposing (..)
import Gui.Nest exposing (..)
import Gui.Grid exposing (..)
import Gui.Mouse exposing (..)
import Gui.Util exposing (..)


type alias Model umsg = ( MouseState, Nest umsg )
type alias View umsg = Grid umsg
type alias Msg umsg = Gui.Msg.Msg umsg


view = Tuple.second >> Gui.Grid.view
--moves mstate = Gui.Mouse.moves mstate >> TrackMouse
moves gui pos = extractMouse gui |> Gui.Mouse.moves pos |> TrackMouse
--ups mstate = Gui.Mouse.ups mstate >> TrackMouse
ups gui pos = extractMouse gui |> Gui.Mouse.ups pos |> TrackMouse
--downs mstate = Gui.Mouse.downs mstate >> TrackMouse
downs gui pos = extractMouse gui |> Gui.Mouse.downs pos |> TrackMouse


extractMouse : Model umsg -> MouseState
extractMouse = Tuple.first


build : Nest umsg -> Model umsg
build nest =
    ( Gui.Mouse.init, nest )


update
    : (umsg -> umodel -> ( umodel, Cmd umsg ))
    -> umodel
    -> Msg umsg
    -> Model umsg
    -> ( ( umodel, Cmd umsg ), Model umsg )
update userUpdate userModel msg ( ( mouse, ui ) as model ) =
    case msg of

        TrackMouse newMouse ->
            update userUpdate userModel
                (findMessageForMouse model newMouse)
                ( newMouse, ui )

        FocusOn pos ->
            ( ( userModel, Cmd.none )
            , ui
                |> shiftFocusTo pos
                |> withMouse mouse
            )

        Tune pos alter ->
            ( ( userModel, Cmd.none )
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Knob label setup curValue handler ->
                                Knob label setup
                                    (alterKnob setup alter curValue)
                                    handler
                            _ -> cell
                    )
                |> withMouse mouse
            )

        ToggleOn pos ->
            ( ( userModel, Cmd.none )
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ handler ->
                                Toggle label TurnedOn handler
                            _ -> cell
                    )
                |> withMouse mouse
            )

        ToggleOff pos ->
            ( ( userModel, Cmd.none )
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ handler ->
                                Toggle label TurnedOff handler
                            _ -> cell
                    )
                |> withMouse mouse
            )

        ExpandNested pos ->
            ( ( userModel, Cmd.none )
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
                |> withMouse mouse
            )

        CollapseNested pos ->
            ( ( userModel, Cmd.none )
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Collapsed cells
                            _ -> cell
                    )
                |> withMouse mouse
            )

        ExpandChoice pos ->
            ( ( userModel, Cmd.none )
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
                |> withMouse mouse
            )

        CollapseChoice pos ->
            ( ( userModel, Cmd.none )
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection handler cells ->
                                Choice label Collapsed selection handler cells
                            _ -> cell
                    )
                |> withMouse mouse
            )

        Select pos ->
            ( ( userModel, Cmd.none )
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
                    |> withMouse mouse
            )

        SendToUser userMsg ->
            ( userUpdate userMsg userModel
            , ui |> withMouse mouse
            )

        SelectAndSendToUser pos userMsg ->
            sequenceUpdate userUpdate userModel
                [ Select pos, SendToUser userMsg ]
                ( ui |> withMouse mouse )

        ToggleOnAndSendToUser pos userMsg ->
            sequenceUpdate userUpdate userModel
                [ ToggleOn pos, SendToUser userMsg ]
                ( ui |> withMouse mouse )

        ToggleOffAndSendToUser pos userMsg ->
            sequenceUpdate userUpdate userModel
                [ ToggleOff pos, SendToUser userMsg ]
                ( ui |> withMouse mouse )

        ShiftFocusLeftAt pos ->
            ( ( userModel, Cmd.none )
            , ui |> shiftFocusBy -1 pos |> withMouse mouse
            )

        ShiftFocusRightAt pos ->
            ( ( userModel, Cmd.none )
            , ui |> shiftFocusBy 1 pos |> withMouse mouse
            )

        TuneAndApply pos alter userMsg ->
            sequenceUpdate userUpdate userModel
                [ Tune pos alter, SendToUser userMsg ]
                ( ui |> withMouse mouse )

        NoOp ->
            ( ( userModel, Cmd.none )
            , ui |> withMouse mouse
            )


withMouse : MouseState -> Nest umsg -> Model umsg
withMouse = Tuple.pair


-- findMessageForMouse : MouseState -> MouseState -> Focus -> Cell umsg -> Msg umsg
-- findMessageForMouse prevMouseState nextMouseState focusedPos focusedCell =
findMessageForMouse : Model umsg -> MouseState -> Msg umsg
findMessageForMouse ( prevMouseState, ui ) nextMouseState =
    let (Focus focusedPos) = findFocus ui
    in
        case findCell focusedPos ui of
            Just (Knob _ knobState curValue handler) ->
                let alter = applyMove prevMouseState nextMouseState knobState curValue
                in
                    if (prevMouseState.down == True && nextMouseState.down == False)
                    then TuneAndApply focusedPos alter
                        <| handler (alterKnob knobState alter curValue)
                    else Tune focusedPos alter
            _ -> NoOp


sequenceUpdate
    : (umsg -> umodel -> ( umodel, Cmd umsg ))
    -> umodel
    -> List (Msg umsg)
    -> Model umsg
    -> ( ( umodel, Cmd umsg ), Model umsg )
sequenceUpdate userUpdate userModel msgs ui =
    List.foldr
        (\msg ( ( prevUserModel, prevCommand ), prevUi ) ->
            let
                ( ( newUserModel, newUserCommand ), newUi ) =
                    update userUpdate prevUserModel msg prevUi
            in
                (
                    ( newUserModel
                    , Cmd.batch [ prevCommand, newUserCommand ]
                    )
                , newUi
                )
        )
        ( ( userModel, Cmd.none ), ui )
        msgs
