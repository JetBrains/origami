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
        productsGrid =
            [ "jetbrains"
            , "intellij"
            , "phpstorm"
            , "pycharm"
            , "rubymine"
            , "webstorm"
            , "clion"
            , "datagrip"
            , "appcode"
            , "goland"
            , "resharper"
            , "resharper-cpp"
            --, "dotcover"
            -- TODO
            ]
                |> List.map ChoiceItem
                |> nest ( 4, 3 )
        sizeGrid =
            [ "window"
            , "1920x1980"
            , "1366x768"
            , "1440x900"
            , "1536x864"
            , "1680x1050"
            ]
                |> List.map ChoiceItem
                |> nest ( 2, 3 )
        webglBlendGrid =
            let
                funcGrid =
                    [ "+", "-", "R-" ]
                        |> List.map ChoiceItem
                        |> nest ( 3, 1 )
                factorGrid =
                    [ "0", "1"
                    , "sC", "1-sC"
                    , "dC", "1-dC"
                    , "sA", "1-sA"
                    , "dA", "1-dA"
                    , "AS"
                    , "CC", "1-CC"
                    , "CA", "1-CA"
                    ]
                        |> List.map ChoiceItem
                        |> nest (8, 2)
            in
                nest ( 3, 2 )
                -- TODO color
                    [ Choice "colorFn" Collapsed 0 funcGrid
                    , Choice "colorFt1" Collapsed 1 factorGrid
                    , Choice "colorFt2" Collapsed 0 factorGrid
                    , Choice "alphaFn" Collapsed 0 funcGrid
                    , Choice "alphaFt1" Collapsed 1 factorGrid
                    , Choice "alphaFt2" Collapsed 0 factorGrid
                    ]
        svgBlendGrid =
            [ "normal"
            , "overlay"
            , "multiply"
            , "darken"
            , "lighten"
            , "multiply"
            , "multiply"
            , "multiply"
            , "multiply"
            ]
                |> List.map ChoiceItem
                |> nest ( 3, 3 )
        amplitudeGrid = noChildren
        fssControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Toggle "mirror" TurnedOff
                , Knob "lights" 0
                , Knob "col" 0
                , Knob "row" 0
                , Nested "fog" Collapsed <|
                    nest ( 2, 1 )
                        [ Knob "shine" 0
                        , Knob "density" 0
                        ]
                , Choice "mesh" Collapsed 0 <|
                    nest ( 2, 1 )
                        [ ChoiceItem "triangles"
                        , ChoiceItem "lines"
                        ]
                , Nested "ranges" Collapsed <|
                        nest ( 3, 1 )
                            [ Knob "horizontal" 0
                            , Knob "vertical" 0
                            , Knob "depth" 0
                            ]
                , Nested "hsb" Collapsed <|
                    nest ( 3, 1 )
                        [ Knob "hue" 0
                        , Knob "saturation" 0
                        , Knob "brightness" 0
                        ]
                , Nested "blend" Collapsed webglBlendGrid
                ]
        svgControls =
            oneLine
                [ Toggle "visible" TurnedOn
                , Choice "blend" Collapsed 0 svgBlendGrid
                ]
    in
        oneLine
            [ Choice "product" Collapsed 0 productsGrid
            , Knob "rotation" 0
            , Choice "size" Collapsed 0 sizeGrid
            , Button "save png" <| always ()
            , Button "lucky" <| always ()
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
