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
import Mouse exposing (Position)
import Time exposing (Time)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Gui.Gui as Gui
import Gui.Def exposing (..)
import Gui.Msg exposing (..)
import Gui.Cell exposing (..)
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
    | ChangeLightSpeed LayerIndex Int
    | ChangeVignette LayerIndex FSS.Vignette
    | ChangeIris LayerIndex FSS.Iris
    | ChangeAmplitude LayerIndex FSS.AmplitudeChange
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
        defaultKnobSetup = { min = 0.0, max = 1.0, step = 0.05, roundBy = 100 }
        productsGrid =
            products
                |> List.map ChoiceItem
                |> nest ( 4, 3 )
        sizeGrid =
            ( "window" :: Dict.keys sizePresets )
                |> List.map ChoiceItem
                |> nest ( 2, 3 )
        webglBlendGrid currentBlend layerIndex =
            let
                funcGrid =
                    blendFuncs
                        |> List.map ChoiceItem
                        |> nest ( 3, 1 )
                factorGrid =
                    blendFactors
                        |> List.map ChoiceItem
                        |> nest (8, 2)
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
        svgBlendGrid =
            svgBlends
                |> List.map ChoiceItem
                |> nest ( 3, 3 )
        amplitudeGrid = noChildren
        fssControls fssModel currentBlend layerIndex =
            let
                { lightSpeed, faces } = fssModel
                ( facesX, facesY ) = faces
                lightSpeedSetup = { min = 0.0, max = 1000.0, step = 1.0, roundBy = 1 }
                facesKnobSetup = { min = 0.0, max = 100.0, step = 1.0, roundBy = 1 }
                changeFacesX val = NoOp
                changeFacesY val = NoOp
                changeAmplutudeX val = NoOp
                changeAmplutudeY val = NoOp
                changeAmplutudeZ val = NoOp
                changeHue val = NoOp
                changeSaturation val = NoOp
                changeBrightness val = NoOp
            in
                oneLine
                    [ Toggle "visible" TurnedOn <| toggleVisibility layerIndex
                    , Toggle "mirror" TurnedOff <| toggleMirror layerIndex
                    , Knob "lights" lightSpeedSetup (toFloat lightSpeed)
                        <| round >> ChangeLightSpeed layerIndex
                    , Knob "col" facesKnobSetup (toFloat facesX) changeFacesX
                    , Knob "row" facesKnobSetup (toFloat facesY) changeFacesY
                    , Nested "fog" Collapsed <|
                        nest ( 2, 1 )
                            [ Knob "shine" defaultKnobSetup 0 <| ChangeVignette layerIndex
                            , Knob "density" defaultKnobSetup 0 <| ChangeIris layerIndex
                            ]
                    , Choice "mesh" Collapsed 0 (chooseMesh layerIndex) <|
                        nest ( 2, 1 )
                            [ ChoiceItem "triangles"
                            , ChoiceItem "lines"
                            ]
                    , Nested "ranges" Collapsed <|
                            nest ( 3, 1 )
                                [ Knob "horizontal" defaultKnobSetup 0 changeAmplutudeX
                                , Knob "vertical" defaultKnobSetup 0 changeAmplutudeY
                                , Knob "depth" defaultKnobSetup 0 changeAmplutudeZ
                                ]
                    , Nested "hsb" Collapsed <|
                        nest ( 3, 1 )
                            [ Knob "hue" defaultKnobSetup 0 changeHue
                            , Knob "saturation" defaultKnobSetup 0 changeSaturation
                            , Knob "brightness" defaultKnobSetup 0 changeBrightness
                            ]
                    , Nested "blend" Collapsed (webglBlendGrid currentBlend layerIndex)
                    ]
        svgControls currentBlend layerIndex =
            oneLine
                [ Toggle "visible" TurnedOn <| toggleVisibility layerIndex
                , Choice "blend" Collapsed 0 (chooseSvgBlend layerIndex) svgBlendGrid
                ]
        toggleMirror layerIndex state =
            layerIndex |> if (state == TurnedOn) then MirrorOn else MirrorOff
        toggleVisibility layerIndex state =
            layerIndex |> if (state == TurnedOn) then TurnOn else TurnOff
        chooseMesh layerIndex _ label =
            FSS.decodeRenderMode label |> ChangeFssRenderMode layerIndex
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
        Gui.build <|
            oneLine <|
                [ Choice "product" Collapsed 0 chooseProduct productsGrid
                , Knob "rotation" defaultKnobSetup 0 Rotate
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
                                            <| fssControls fssModel webglBlend layerIndex
                                    _ -> Ghost <| "layer " ++ toString layerIndex
                            SVGLayer _ svgBlend ->
                                Nested (String.toLower name) Collapsed
                                    <| svgControls svgBlend layerIndex
                    )
                    from.layers

