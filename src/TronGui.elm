module TronGui exposing (gui)

import Dict as Dict

import Gui.Gui as Gui
import Gui.Def exposing (..)
import Gui.Nest exposing (..)

import Layer.FSS as FSS

import WebGL.Blend as WGLBlend
import Html.Blend as HtmlBlend

import Product as Product
import Model exposing (..)

gui : Model -> Gui.Model Msg
gui from =
    let
        ( currentSizePresets, sizePresetsShape ) =
            ( getSizePresets from.mode
                |> List.map (\preset -> ( getPresetLabel preset, getPresetSize preset ))
                |> Dict.fromList
            , case from.mode of
                Release -> ( 4, 4 )
                Ads -> ( 8, 4 )
                _ -> ( 4, 2 )
            )
        products =
            [ "jetbrains"
            , "idea"
            , "phpstorm"
            , "pycharm"
            , "rubymine"
            , "webstorm"
            , "clion"
            , "datagrip"
            , "appcode"
            , "goland"
            , "rs"
            , "rs cpp"
            , "dotcover"
            , "dotmemory"
            , "dotpeek"
            , "dottrace"
            , "rider"
            , "teamcity"
            , "youtrack"
            , "upsource"
            , "hub"
            , "kotlin"
            , "mps"
            -- TODO
            ]
        htmlBlends =
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
        productsGrid =
            products
                |> List.map ChoiceItem
                |> nestWithin ( 6, 4 )
        sizeGrid =
            ( "browser" :: Dict.keys currentSizePresets )
                |> List.map ChoiceItem
                |> nestWithin sizePresetsShape
        htmlBlendGrid =
            htmlBlends
                |> List.map ChoiceItem
                |> nestWithin ( 3, 3 )
        htmlControls currentBlend layerIndex =
            oneLine
                [ Toggle "visible" TurnedOn <| toggleVisibility layerIndex
                , Choice "blend" Collapsed 0 (chooseHtmlBlend layerIndex) htmlBlendGrid
                ]
        chooseProduct _ label =
            case label of
                "rs" -> ChangeProduct Product.ReSharper
                "rs cpp" -> ChangeProduct Product.ReSharperCpp
                "idea" -> ChangeProduct Product.IntelliJ
                _ -> ChangeProduct <| Product.decode label
        chooseSize _ label =
            case label of
                "window" -> RequestFitToWindow
                "browser" -> RequestFitToWindow
                _ ->
                    currentSizePresets
                        |> Dict.get label
                        |> Maybe.map (\(w, h) -> ResizeFromPreset <| ViewportSize w h)
                        |> Maybe.withDefault RequestFitToWindow
        chooseWebGlBlend layerIndex index label =
            NoOp
        chooseHtmlBlend layerIndex _ label =
            ChangeHtmlBlend layerIndex <| HtmlBlend.decode label
        toggleVisibility layerIndex state =
            layerIndex |> if (state == TurnedOn) then TurnOn else TurnOff
        rotateKnobSetup =
            { min = -1.0, max = 1.0, step = 0.05, roundBy = 100
            , default = from.omega }
        layerButtons =
            from.layers
                |> List.filter
                    (\{ name } ->
                        case ( name, from.mode ) of
                            ( "Cover", Production ) -> False
                            _ -> True
                    )
                |> List.indexedMap
                    (\layerIndex { name, layer, model } ->
                        case layer of
                            WebGLLayer webGllayer webglBlend ->
                                case model of
                                    FssModel fssModel ->
                                        Nested (String.toLower name) Collapsed
                                            <| fssControls from.mode fssModel webglBlend layerIndex
                                    _ -> Ghost <| "layer " ++ String.fromInt layerIndex
                            HtmlLayer _ htmlBlend ->
                                Nested (String.toLower name) Collapsed
                                    <| htmlControls htmlBlend layerIndex
                    )
    in
        Gui.build <|
            oneLine <|
                [ Choice "product" Collapsed 0 chooseProduct productsGrid
                , Knob "rotation" rotateKnobSetup from.omega Rotate
                , Choice "size" Collapsed 0 chooseSize sizeGrid
                -- , Button "save png" <| always SavePng
                , Button "lucky" <| always Randomize
                ]
                ++ layerButtons



webglBlendGrid : UiMode -> WGLBlend.Blend -> LayerIndex -> Nest Msg
webglBlendGrid mode currentBlend layerIndex =
    let
        blendFuncs =
            [ "+", "-", "R-" ]
        blendFactors =
            [ "0", "1"
            , "sC", "1-sC"
            , "dC", "1-dC"
            , "sα", "1-sα"
            , "dα", "1-dα"
            , "αS"
            , "CC", "1-CC"
            , "Cα", "1-Cα"
            ]
        funcGrid =
            blendFuncs
                |> List.map ChoiceItem
                |> nestWithin ( 3, 1 )
        factorGrid =
            blendFactors
                |> List.map ChoiceItem
                |> nestWithin ( 8, 2 )
        chooseBlendColorFn index label =
            AlterWGLBlend layerIndex
                (\curBlend ->
                    let ( _, colorFactor1, colorFactor2 ) = curBlend.colorEq
                    in { curBlend | colorEq =
                        ( WGLBlend.decodeFunc label, colorFactor1, colorFactor2 ) }
                )
        chooseBlendColorFact1 index label =
            AlterWGLBlend layerIndex
                (\curBlend ->
                    let ( colorFunc, _, colorFactor2 ) = curBlend.colorEq
                    in { curBlend | colorEq =
                        ( colorFunc, WGLBlend.decodeFactor label, colorFactor2 ) }
                )
        chooseBlendColorFact2 index label =
            AlterWGLBlend layerIndex
                (\curBlend ->
                    let ( colorFunc, colorFactor1, _ ) = curBlend.colorEq
                    in { curBlend | colorEq =
                        ( colorFunc, colorFactor1, WGLBlend.decodeFactor label ) }
                )
        chooseBlendAlphaFn index label =
            AlterWGLBlend layerIndex
                (\curBlend ->
                    let ( _, alphaFactor1, alphaFactor2 ) = curBlend.alphaEq
                    in { curBlend | alphaEq =
                        ( WGLBlend.decodeFunc label, alphaFactor1, alphaFactor2 ) }
                )
        chooseBlendAlphaFact1 index label =
            AlterWGLBlend layerIndex
                (\curBlend ->
                    let ( alphaFunc, alphaFactor1, _ ) = curBlend.alphaEq
                    in { curBlend | alphaEq =
                        ( alphaFunc, alphaFactor1, WGLBlend.decodeFactor label )
                    }
                )
        chooseBlendAlphaFact2 index label =
            AlterWGLBlend layerIndex
                (\curBlend ->
                    let ( alphaFunc, _, alphaFactor2 ) = curBlend.alphaEq
                    in { curBlend | alphaEq =
                        ( alphaFunc, WGLBlend.decodeFactor label, alphaFactor2 )
                    }
                )
    in
        nestWithin ( 3, 2 )
        -- TODO color
            [ Choice "colorFn"  Collapsed 0 chooseBlendColorFn funcGrid
            , Choice "colorFt1" Collapsed 1 chooseBlendColorFact1 factorGrid
            , Choice "colorFt2" Collapsed 0 chooseBlendColorFact2 factorGrid
            , Choice "alphaFn"  Collapsed 0 chooseBlendAlphaFn funcGrid
            , Choice "alphaFt1" Collapsed 1 chooseBlendAlphaFact1 factorGrid
            , Choice "alphaFt2" Collapsed 0 chooseBlendAlphaFact2 factorGrid
            ]


fssControls : UiMode -> FSS.Model -> WGLBlend.Blend -> LayerIndex -> Nest Msg
fssControls mode fssModel currentBlend layerIndex =
    let
        { lightSpeed, faces, amplitude, vignette, iris, colorShift } = fssModel
        { amplitudeX, amplitudeY, amplitudeZ } = amplitude
        changeFacesX val = AlterFaces layerIndex
                                    { xChange = Just <| round val, yChange = Nothing }
        changeFacesY val = AlterFaces layerIndex
                                    { xChange = Nothing, yChange = Just <| round val }
        changeAmplutudeX val = AlterAmplitude layerIndex
                                    <| FSS.AmplitudeChange (Just val) Nothing Nothing
        changeAmplutudeY val = AlterAmplitude layerIndex
                                    <| FSS.AmplitudeChange Nothing (Just val) Nothing
        changeAmplutudeZ val = AlterAmplitude layerIndex
                                    <| FSS.AmplitudeChange Nothing Nothing (Just val)
        changeHue val = ShiftColor layerIndex
                                    <| FSS.ColorShiftPatch (Just val) Nothing Nothing
        changeSaturation val = ShiftColor layerIndex
                                    <| FSS.ColorShiftPatch Nothing (Just val) Nothing
        changeBrightness val = ShiftColor layerIndex
                                    <| FSS.ColorShiftPatch Nothing Nothing (Just val)
        toggleMirror state =
            layerIndex |> if (state == TurnedOn) then MirrorOn else MirrorOff
        toggleVisibility state =
            layerIndex |> if (state == TurnedOn) then TurnOn else TurnOff
        chooseMesh _ label =
            FSS.decodeRenderMode label |> ChangeFssRenderMode layerIndex
        defaultKnobSetup defaultVal =
            { min = 0.0, max = 1.0, step = 0.05, roundBy = 100, default = defaultVal }
        lightSpeedSetup =
            { min = 0.0, max = 2000.0, step = 1.0, roundBy = 1
            , default = toFloat lightSpeed }
        facesKnobSetup defaultVal =
            { min = 0.0, max = 100.0, step = 1.0, roundBy = 1, default = defaultVal }
        colorShiftKnobSetup defaultVal =
            { min = -1.0, max = 1.0, step = 0.05, roundBy = 100, default = defaultVal }
    in
        oneLine
            [ Toggle "visible" TurnedOn toggleVisibility
            , Toggle "mirror" TurnedOff toggleMirror
            -- , Knob "opacity" TurnedOff <| toggleMirror layerIndex
            , Knob "lights" lightSpeedSetup (toFloat lightSpeed)
                <| round >> ChangeLightSpeed layerIndex
            , Knob "col"
                (facesKnobSetup <| toFloat faces.x)
                (toFloat faces.y)
                changeFacesX
            , Knob "row"
                (facesKnobSetup <| toFloat faces.y)
                (toFloat faces.y)
                changeFacesY
            , Nested "fog" Collapsed <|
                nestWithin ( 2, 1 )
                    [ Knob "shine"
                        (defaultKnobSetup vignette)
                        vignette <| ChangeVignette layerIndex
                    , Knob "density"
                        (defaultKnobSetup iris)
                        iris <| ChangeIris layerIndex
                    ]
            , Choice "mesh" Collapsed 0 chooseMesh <|
                nestWithin ( 2, 1 )
                    [ ChoiceItem "triangles"
                    , ChoiceItem "lines"
                    ]
            , Nested "ranges" Collapsed <|
                    nestWithin ( 3, 1 )
                        [ Knob "horizontal"
                            (defaultKnobSetup amplitudeX)
                            amplitudeX changeAmplutudeX
                        , Knob "vertical"
                            (defaultKnobSetup amplitudeY)
                            amplitudeY changeAmplutudeY
                        , Knob "depth"
                            (defaultKnobSetup amplitudeZ)
                            amplitudeZ changeAmplutudeZ
                        ]
            , Nested "hsb" Collapsed <|
                nestWithin ( 3, 1 )
                    [ Knob "hue"
                        (colorShiftKnobSetup colorShift.hue)
                        colorShift.hue changeHue
                    , Knob "saturation"
                        (colorShiftKnobSetup colorShift.saturation)
                        colorShift.saturation changeSaturation
                    , Knob "brightness"
                        (colorShiftKnobSetup colorShift.brightness)
                        colorShift.brightness changeBrightness
                    ]
            , Nested "blend" Collapsed
                <| webglBlendGrid mode currentBlend layerIndex
            ]

