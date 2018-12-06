module Gui.Gui exposing
    ( Msg, Model
    , view, update, build
    , moves, ups, downs
    , extractMouse
    )


import Gui.Cell exposing (..)
import Gui.Nest exposing (..)
import Gui.Grid exposing (..)
import Gui.Mouse exposing (..)


type alias Model umsg = ( MouseState, Nest umsg )
type alias View umsg = Grid umsg
type alias Msg umsg = Gui.Cell.Msg umsg


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


subscriptions : Model umsg -> Sub (Msg umsg)
subscriptions model = Sub.batch []


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

        Tune pos alter ->
            ( userModel ! []
            , ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Knob label setup curValue ->
                                Knob label setup
                                    <| alterKnob setup alter curValue
                            _ -> cell
                    )
                |> withMouse mouse
            )

        ToggleOn pos ->
            ( userModel ! []
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
            ( userModel ! []
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
                |> withMouse mouse
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
                |> withMouse mouse
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
                |> withMouse mouse
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
                |> withMouse mouse
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
            ( userModel ! []
            , ui |> shiftFocusBy -1 pos |> withMouse mouse
            )

        ShiftFocusRightAt pos ->
            ( userModel ! []
            , ui |> shiftFocusBy 1 pos |> withMouse mouse
            )

        NoOp ->
            ( userModel ! []
            , ui |> withMouse mouse
            )


withMouse : MouseState -> Nest umsg -> Model umsg
withMouse = (,)


applyMove : MouseState -> MouseState -> AlterKnob
applyMove prev next =
    let
        ( prevX, prevY ) = prev.vec
        ( nextX, nextY ) = next.vec
    in
        if (nextY == 0.0) then Stay
            else if (nextY < 0.0) then Down
                else if (nextY > 0.0) then Up
                    else Stay



alterKnob : KnobState -> AlterKnob -> Float -> Float
alterKnob { min, max, step } (Alter amount) curValue =
    -- amount is a (-0.5 <= value <= 0.5)
    let
        fullValue = if amount < 0 then (amount + 0.5)
    in case direction of
        Up -> if (curValue + step < max) then curValue + step else max
        Down -> if (curValue - step > min) then curValue - step else min
        Stay -> curValue


findMessageForMouse : Model umsg -> MouseState -> Msg umsg
findMessageForMouse ( prevState, ui ) nextState =
    let (Focus focusedPos) = findFocus ui
    in
        case findCell focusedPos ui of
            Just (Knob _ _ _) ->
                Tune focusedPos <| applyMove prevState nextState
            _ -> NoOp


sequenceUpdate
    : (umsg -> umodel -> ( umodel, Cmd umsg ))
    -> umodel
    -> List (Msg umsg)
    -> Model umsg
    -> ( ( umodel, Cmd umsg ), Model umsg )
sequenceUpdate userUpdate userModel msgs ui =
    List.foldr
        (\msg ( ( userModel, prevCommand ), ui ) ->
            let
                ( ( newUserModel, newUserCommand ), newUi ) =
                    update userUpdate userModel msg ui
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
