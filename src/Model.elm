module Model exposing
    ( init
    , initEmpty
    , Model
    , UiMode(..)
    , Layer
    , emptyLayer
    , LayerIndex
    , LayerDef
    , LayerModel(..)
    , LayerKind(..)
    , Layer(..)
    , WebGLLayer_(..)
    , SVGLayer_(..)
    , CreateLayer
    , Size
    , Pos
    , PortModel
    , PortLayerDef
    , PortBlend
    , Msg(..)
    , gui
    )


import Dict as Dict
import Window
import Time exposing (Time)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Gui.Gui as Gui
import Gui.Def exposing (..)
import Gui.Nest exposing (..)

import Product exposing (Product)
import Product
import Gui.Gui as Gui
import Layer.FSS as FSS
import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.Template as Template
import Layer.Vignette as Vignette


type alias LayerIndex = Int

type alias Size = (Int, Int)
type alias Pos = (Int, Int)

type alias CreateLayer = LayerKind -> LayerModel -> Layer


type Msg
    = Bang
    | Animate Time
    | GuiMessage (Gui.Msg Msg)
    | Resize Window.Size
    | ResizeFromPreset Window.Size
    | Locate Pos
    | Rotate Float
    | Import String
    | Export
    | ExportZip
    | TimeTravel Float
    | BackToNow
    | Pause
    | Continue
    | TriggerPause
    | HideControls
    | ChangeProduct Product
    | TurnOn LayerIndex
    | TurnOff LayerIndex
    | MirrorOn LayerIndex
    | MirrorOff LayerIndex
    | Configure LayerIndex LayerModel
    | ChangeWGLBlend LayerIndex WGLBlend.Blend
    | ChangeSVGBlend LayerIndex SVGBlend.Blend
    | RebuildFss LayerIndex FSS.SerializedScene
    --| RebuildOnClient LayerIndex FSS.SerializedScene
    | ChangeFssRenderMode LayerIndex FSS.RenderMode
    | ChangeFaces LayerIndex ( Int, Int )
    | AlterFaces LayerIndex FSS.FacesChange
    | ChangeLightSpeed LayerIndex Int
    | ChangeVignette LayerIndex FSS.Vignette
    | ChangeIris LayerIndex FSS.Iris
    | AlterAmplitude LayerIndex FSS.AmplitudeChange
    | ShiftColor LayerIndex FSS.ColorShiftPatch
    | ChangeOpacity LayerIndex FSS.Opacity
    | Randomize
    | ApplyRandomizer PortModel
    | SavePng
    | NoOp


type UiMode
    = Development
    | Production
    | Release
    | Ads


type LayerKind
    = Lorenz
    | Fractal
    | Template
    | Voronoi
    | Fss
    | MirroredFss
    | Cover
    | Vignette
    | Empty


-- type LayerBlend
--     = WGLB WGLBlend.Blend
--     | SVGB SVGBlend.Blend


type LayerModel
    = LorenzModel Lorenz.Model
    | FractalModel Fractal.Model
    | VoronoiModel Voronoi.Model
    | FssModel FSS.Model
    | TemplateModel Template.Model
    | VignetteModel Vignette.Model
    | NoModel


type WebGLLayer_
    = LorenzLayer Lorenz.Mesh
    | FractalLayer Fractal.Mesh
    | VoronoiLayer Voronoi.Mesh
    | TemplateLayer Template.Mesh
    | FssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | MirroredFssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | VignetteLayer


type SVGLayer_
    = CoverLayer
    | NoContent


type Layer
    = WebGLLayer WebGLLayer_ WGLBlend.Blend
    | SVGLayer SVGLayer_ SVGBlend.Blend


-- `change` is needed since we store a sample layer model
-- to use for any layer in the main model
type alias LayerDef =
    { kind : LayerKind
    , name : String
    , layer : Layer
    , model : LayerModel
    , on : Bool
    }


type alias Model =
    { background: String
    , mode : UiMode
    , gui : Maybe (Gui.Model Msg)
    , paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , omega : Float
    , layers : List LayerDef
    , size : Size
    , origin : Pos
    , mouse : Pos
    , now : Time
    , timeShift : Time
    , product : Product
    , controlsVisible : Bool
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }

-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> SVG Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend =
    ( Maybe WGLBlend.Blend, Maybe String )


type alias PortModel =
    { background : String
    , layers : List PortLayerDef
    , mode : String
    , mouse : ( Int, Int )
    , now : Time.Time
    , origin : Pos
    , size : Size
    , theta : Float
    , omega : Float
    , product : String
    , palette : Product.Palette
    }


type alias PortLayerDef =
    { kind : String
    , blend : PortBlend
    , webglOrSvg : String
    , isOn : Bool
    , name : String
    , model : String
    }


init
    :  UiMode
    -> List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> Model
init mode initialLayers createLayer =
    let
        emptyModel = initEmpty mode
        modelWithLayers =
            { emptyModel
            | layers = initialLayers |> List.map
                (\(kind, name, layerModel) ->
                    { kind = kind
                    , layer = layerModel |> createLayer kind
                    , name = name
                    , model = layerModel
                    , on = True
                    })
            }
    in
        { modelWithLayers
        | gui = Just <| gui modelWithLayers
        }


initEmpty : UiMode -> Model
initEmpty mode =
    { background = "#333"
    , mode = mode
    , gui = Nothing
    , paused = False
    , autoRotate = False
    , fps = 0
    , theta = 0.1
    , omega = 0.0
    , layers = []
    , size = ( 1200, 1200 )
    , origin = ( 0, 0 )
    , mouse = ( 0, 0 )
    , now = 0.0
    , timeShift = 0.0
    --, range = ( 0.8, 1.0 )
    , product = Product.JetBrains
    , controlsVisible = True
    }


emptyLayer : Layer
emptyLayer =
    SVGLayer NoContent SVGBlend.default


sizePresets : Dict.Dict String ( Int, Int )
sizePresets =
    Dict.fromList
        [ ( "1920x1980", ( 1920, 1980 ) )
        , ( "1366x768", ( 1366, 769 ) )
        , ( "1440x900", ( 1440, 900 ) )
        , ( "1536x864", ( 1536, 864 ) )
        , ( "1680x1050", ( 1680, 1050 ) )
        ]


gui : Model -> Gui.Model Msg
gui from =
    let
        products =
            [ "jetbrains"
            , "intellij idea"
            , "phpstorm"
            , "pycharm"
            , "rubymine"
            , "webstorm"
            , "clion"
            , "datagrip"
            , "appcode"
            , "goland"
            , "resharper"
            , "resharper c++"
            --, "dotcover"
            -- TODO
            ]
        svgBlends =
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
                |> nest ( 4, 3 )
        sizeGrid =
            ( "window" :: Dict.keys sizePresets )
                |> List.map ChoiceItem
                |> nest ( 2, 3 )
        svgBlendGrid =
            svgBlends
                |> List.map ChoiceItem
                |> nest ( 3, 3 )
        svgControls currentBlend layerIndex =
            oneLine
                [ Toggle "visible" TurnedOn <| toggleVisibility layerIndex
                , Choice "blend" Collapsed 0 (chooseSvgBlend layerIndex) svgBlendGrid
                ]
        chooseProduct _ label =
            case label of
                "resharper c++" -> ChangeProduct Product.ReSharperCpp
                "intellij idea" -> ChangeProduct Product.IntelliJ
                _ -> ChangeProduct <| Product.decode label
        chooseSize _ label =
            sizePresets
                |> Dict.get label
                |> Maybe.map (\(w, h) -> ResizeFromPreset <| Window.Size w h)
                |> Maybe.withDefault NoOp -- TODO: fitWindow
        chooseWebGlBlend layerIndex index label =
            NoOp
        chooseSvgBlend layerIndex _ label =
            ChangeSVGBlend layerIndex <| SVGBlend.decode label
        toggleVisibility layerIndex state =
            layerIndex |> if (state == TurnedOn) then TurnOn else TurnOff
        rotateKnobSetup =
            { min = -1.0, max = 1.0, step = 0.05, roundBy = 100
            , default = from.omega }
    in
        Gui.build <|
            oneLine <|
                [ Choice "product" Collapsed 0 chooseProduct productsGrid
                , Knob "rotation" rotateKnobSetup from.omega Rotate
                , Choice "size" Collapsed 0 chooseSize sizeGrid
                , Button "save png" <| always SavePng
                , Button "lucky" <| always Randomize
                ]
                ++ List.indexedMap
                    (\layerIndex { name, layer, model } ->
                        case layer of
                            WebGLLayer webGllayer webglBlend ->
                                case model of
                                    FssModel fssModel ->
                                        Nested (String.toLower name) Collapsed
                                            <| fssControls from.mode fssModel webglBlend layerIndex
                                    _ -> Ghost <| "layer " ++ toString layerIndex
                            SVGLayer _ svgBlend ->
                                Nested (String.toLower name) Collapsed
                                    <| svgControls svgBlend layerIndex
                    )
                    from.layers


webglBlendGrid : UiMode -> WGLBlend.Blend -> LayerIndex -> Nest Msg
webglBlendGrid mode currentBlend layerIndex =
    let
        blendFuncs =
            [ "+", "-", "R-" ]
        blendFactors =
            [ "0", "1"
            , "sC", "1-sC"
            , "dC", "1-dC"
            , "sA", "1-sA"
            , "dA", "1-dA"
            , "AS"
            , "CC", "1-CC"
            , "CA", "1-CA"
            ]
        funcGrid =
            blendFuncs
                |> List.map ChoiceItem
                |> nest ( 3, 1 )
        factorGrid =
            blendFactors
                |> List.map ChoiceItem
                |> nest (8, 2)
        chooseBlendColorFn layerIndex index label =
            NoOp
        chooseBlendColorFact1 layerIndex index label =
            NoOp
        chooseBlendColorFact2 layerIndex index label =
            NoOp
        chooseBlendAlphaFn layerIndex index label =
            NoOp
        chooseBlendAlphaFact1 layerIndex index label =
            NoOp
        chooseBlendAlphaFact2 layerIndex index label =
            NoOp
    in
        nest ( 3, 2 )
        -- TODO color
            [ Choice "colorFn" Collapsed 0
                (chooseBlendColorFn layerIndex) funcGrid
            , Choice "colorFt1" Collapsed 1
                (chooseBlendColorFact1 layerIndex) factorGrid
            , Choice "colorFt2" Collapsed 0
                (chooseBlendColorFact2 layerIndex) factorGrid
            , Choice "alphaFn" Collapsed 0
                (chooseBlendAlphaFn layerIndex) funcGrid
            , Choice "alphaFt1" Collapsed 1
                (chooseBlendAlphaFact1 layerIndex) factorGrid
            , Choice "alphaFt2" Collapsed 0
                (chooseBlendAlphaFact2 layerIndex) factorGrid
            ]


fssControls : UiMode -> FSS.Model -> WGLBlend.Blend -> LayerIndex -> Nest Msg
fssControls mode fssModel currentBlend layerIndex =
    let
        { lightSpeed, faces, amplitude, vignette, iris, colorShift } = fssModel
        ( facesX, facesY ) = faces
        ( amplitudeX, amplitudeY, amplitudeZ ) = amplitude
        ( hueShift, saturationShift, brightnessShift ) = colorShift
        changeFacesX val = AlterFaces layerIndex ( Just <| round val, Nothing )
        changeFacesY val = AlterFaces layerIndex ( Nothing, Just <| round val )
        changeAmplutudeX val = AlterAmplitude layerIndex ( Just val, Nothing, Nothing )
        changeAmplutudeY val = AlterAmplitude layerIndex ( Nothing, Just val, Nothing )
        changeAmplutudeZ val = AlterAmplitude layerIndex ( Nothing, Nothing, Just val )
        changeHue val = ShiftColor layerIndex ( Just val, Nothing, Nothing )
        changeSaturation val = ShiftColor layerIndex ( Nothing, Just val, Nothing )
        changeBrightness val = ShiftColor layerIndex ( Nothing, Nothing, Just val )
        toggleMirror layerIndex state =
            layerIndex |> if (state == TurnedOn) then MirrorOn else MirrorOff
        toggleVisibility layerIndex state =
            layerIndex |> if (state == TurnedOn) then TurnOn else TurnOff
        chooseMesh layerIndex _ label =
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
            [ Toggle "visible" TurnedOn <| toggleVisibility layerIndex
            , Toggle "mirror" TurnedOff <| toggleMirror layerIndex
            , Knob "lights" lightSpeedSetup (toFloat lightSpeed)
                <| round >> ChangeLightSpeed layerIndex
            , Knob "col"
                (facesKnobSetup <| toFloat facesX)
                (toFloat facesX)
                changeFacesX
            , Knob "row"
                (facesKnobSetup <| toFloat facesY)
                (toFloat facesY)
                changeFacesY
            , Nested "fog" Collapsed <|
                nest ( 2, 1 )
                    [ Knob "shine"
                        (defaultKnobSetup vignette)
                        vignette <| ChangeVignette layerIndex
                    , Knob "density"
                        (defaultKnobSetup iris)
                        iris <| ChangeIris layerIndex
                    ]
            , Choice "mesh" Collapsed 0 (chooseMesh layerIndex) <|
                nest ( 2, 1 )
                    [ ChoiceItem "triangles"
                    , ChoiceItem "lines"
                    ]
            , Nested "ranges" Collapsed <|
                    nest ( 3, 1 )
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
                nest ( 3, 1 )
                    [ Knob "hue"
                        (colorShiftKnobSetup hueShift)
                        hueShift changeHue
                    , Knob "saturation"
                        (colorShiftKnobSetup saturationShift)
                        saturationShift changeSaturation
                    , Knob "brightness"
                        (colorShiftKnobSetup brightnessShift)
                        brightnessShift changeBrightness
                    ]
            , Nested "blend" Collapsed
                <| webglBlendGrid mode currentBlend layerIndex
            ]
