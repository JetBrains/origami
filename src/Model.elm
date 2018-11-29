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
    )


import Time exposing (Time)
import Window
import Mouse exposing (Position)

import Time exposing (Time)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Gui.Gui as Gui
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
    | Locate Position
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


-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> SVG Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend =
    ( Maybe WGLBlend.Blend, Maybe String )


type alias Model =
    { background: String
    , mode : UiMode
    , gui : Gui.Model Msg
    , paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , omega : Float
    , layers : List LayerDef
    , size : Size
    , origin : Pos
    , mouse : (Int, Int)
    , now : Time
    , timeShift : Time
    , product : Product
    , controlsVisible : Bool
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }


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


initEmpty : UiMode -> Model
initEmpty mode =
    { background = "#333"
    , mode = mode
    , gui = gui
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


init
    :  UiMode
    -> List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> Model
init mode initialLayers createLayer =
    let
        initialModel = initEmpty mode
    in
        { initialModel
        | layers = initialLayers |> List.map
            (\(kind, name, layerModel) ->
                { kind = kind
                , layer = layerModel |> createLayer kind
                , name = name
                , model = layerModel
                , on = True
                })
        }


emptyLayer : Layer
emptyLayer =
    SVGLayer NoContent SVGBlend.default


gui : Gui.Model Msg
gui =
    let
        productsGrid =
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
                    [ Choice "colorFn" Collapsed 0 chooseBlendColorFn funcGrid
                    , Choice "colorFt1" Collapsed 1 chooseBlendColorFact1 factorGrid
                    , Choice "colorFt2" Collapsed 0 chooseBlendColorFact2 factorGrid
                    , Choice "alphaFn" Collapsed 0 chooseBlendAlphaFn funcGrid
                    , Choice "alphaFt1" Collapsed 1 chooseBlendAlphaFact1 factorGrid
                    , Choice "alphaFt2" Collapsed 0 chooseBlendAlphaFact2 factorGrid
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
                , Choice "mesh" Collapsed 0 chooseMesh <|
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
                , Choice "blend" Collapsed 0 chooseSvgBlend svgBlendGrid
                ]
        logMsg_ name index label v =
            -- Debug.log (name ++ " " ++ toString index ++ " " ++ label) v
            v
        chooseMesh index label _ =
            logMsg_ "mesh" index label <| NoOp
        chooseProduct index label _ =
            logMsg_ "product" index label
                <| case label of
                    "resharper c++" -> ChangeProduct Product.ReSharperCpp
                    "intellij idea" -> ChangeProduct Product.IntelliJ
                    _ -> ChangeProduct <| Product.decode label
        chooseSize index label _ =
            logMsg_ "size" index label <| NoOp
        chooseWebGlBlend index label _ =
            logMsg_ "wglblend" index label <| NoOp
        chooseSvgBlend index label _ =
            logMsg_ "svgBlend" index label <| NoOp
        chooseBlendColorFn index label _ =
            logMsg_ "blendColorFn" index label <| NoOp
        chooseBlendColorFact1 index label _ =
            logMsg_ "blendColorFact1" index label <| NoOp
        chooseBlendColorFact2 index label _ =
            logMsg_ "blendColorFact2" index label <| NoOp
        chooseBlendAlphaFn index label _ =
            logMsg_ "blendAlphaFn" index label <| NoOp
        chooseBlendAlphaFact1 index label _ =
            logMsg_ "blendAlphaFact1" index label <| NoOp
        chooseBlendAlphaFact2 index label _ =
            logMsg_ "blendAlphaFact2" index label <| NoOp
    in
        oneLine
            [ Choice "product" Collapsed 0 chooseProduct productsGrid
            , Knob "rotation" 0
            , Choice "size" Collapsed 0 chooseSize sizeGrid
            , Button "save png" <| always SavePng
            , Button "lucky" <| always NoOp
            , Nested "logo" Collapsed svgControls
            , Nested "title" Collapsed svgControls
            , Nested "net" Collapsed fssControls
            , Nested "low-poly" Collapsed fssControls
            ]
