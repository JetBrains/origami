module Gui.Gui exposing
    ( Msg
    , Model
    , view
    , update
    , init
    )


import Gui.Model exposing (..)
import Gui.View exposing (..)


type alias Msg = Gui.Model.Msg
type alias Model = Gui.Model.Model
view = Gui.View.view


init : Model -- ( UI, Cmd Msg )
init =
    let
        webglBlendGrid = noChildren
        svgBlendGrid =
            ( ( 3, 3 )
            ,
                [ ChoiceItem "normal"
                , ChoiceItem "overlay"
                , ChoiceItem "multiply"
                , ChoiceItem "darken"
                , ChoiceItem "lighten"
                , ChoiceItem "multiply"
                , ChoiceItem "multiply"
                , ChoiceItem "multiply"
                , ChoiceItem "multiply"
                ]
            )

        amplitudeGrid = noChildren
        fssControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOff
                , Knob "lights" 0
                , Knob "col" 0
                , Knob "vignette" 0
                , Knob "iris" 0
                , Choice "mesh" Collapsed 0 noChildren
                , Nested "amplitude" Collapsed amplitudeGrid
                , Nested "blend" Collapsed webglBlendGrid
                ]
        svgControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Choice "blend" Collapsed 0 svgBlendGrid
                ]
    in
        oneLine
            [ Choice "product" Collapsed 0 noChildren
            , Knob "rotation" 0
            , Choice "size" Collapsed 0 noChildren
            , Button "save png" <| always ()
            , Button "save batch" <| always ()
            , Nested "logo" Collapsed svgControls
            , Nested "title" Collapsed svgControls
            , Nested "net" Collapsed fssControls
            , Nested "low-poly" Collapsed fssControls
            ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch []


update : Msg -> Model -> Model -- ( UI, Cmd Msg )
update msg ui =
    case msg of
        Tune pos value ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Knob label _ -> Knob label value
                            _ -> cell
                    )
        On pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOn
                            _ -> cell
                    )
        Off pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ -> Toggle label TurnedOff
                            _ -> cell
                    )
        ExpandNested pos ->
            ui
                |> collapseAllAbove pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Expanded cells
                            _ -> cell
                    )
        CollapseNested pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Collapsed cells
                            _ -> cell
                    )
        ExpandChoice pos ->
            ui
                |> collapseAllAbove pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection cells ->
                                Choice label Expanded selection cells
                            _ -> cell
                    )
        CollapseChoice pos ->
            ui |>
                updateCell pos
                    (\cell ->
                        case cell of
                            Choice label _ selection cells ->
                                Choice label Collapsed selection cells
                            _ -> cell
                    )
        Select parentPos ((ModelPos _ _ index) as pos) ->
            ui |> updateCell parentPos
                    (\cell ->
                        case cell of
                            Choice label expanded selection cells ->
                                Choice label expanded index cells
                            _ -> cell
                    )
        _ -> ui
