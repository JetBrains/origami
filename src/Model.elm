module Model exposing
    ( init
    , initEmpty
    , Model
    , UiMode(..)
    , Layer(..)
    , emptyLayer
    , LayerIndex
    , LayerDef
    , LayerModel(..)
    , LayerKind(..)
    , WebGLLayer_(..)
    , HtmlLayer_(..)
    , CreateLayer
    , ViewportSize(..)
    , Size
    , Pos
    , TimeDelta
    , PortModel
    , PortLayerDef
    , PortBlend
    , Msg(..)
    , gui
    )


import Dict as Dict
import Time as Time
import Browser.Dom exposing (Viewport)

import WebGL.Blend as WGLBlend
import Html.Blend as HtmlBlend

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


type ViewportSize = ViewportSize Int Int
type alias Size = (Int, Int)
type alias Pos = (Int, Int)
type alias TimeNow = Float
type alias TimeDelta = Float

type alias CreateLayer = LayerKind -> LayerModel -> Layer


type Msg
    = Bang
    | ChangeMode UiMode
    | Animate TimeDelta
    | GuiMessage (Gui.Msg Msg)
    | Resize ViewportSize
    | ResizeFromPreset ViewportSize
    | RequestFitToWindow
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
    | AlterWGLBlend LayerIndex WGLBlend.BlendChange
    | ChangeHtmlBlend LayerIndex HtmlBlend.Blend
    | RebuildFss LayerIndex FSS.SerializedScene
    --| RebuildOnClient LayerIndex FSS.SerializedScene
    | ChangeFssRenderMode LayerIndex FSS.RenderMode
    | ChangeFaces LayerIndex FSS.Faces
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
    | TronUi UiMode


type LayerKind
    = Lorenz
    | Fractal
    | Template
    | Canvas
    | Voronoi
    | Fss
    | MirroredFss
    | Cover
    | Vignette
    | Empty


-- type LayerBlend
--     = WGLB WGLBlend.Blend
--     | HTMLB HtmlBlend.Blend


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


type HtmlLayer_
    = CoverLayer
    | CanvasLayer
    | NoContent


type Layer
    = WebGLLayer WebGLLayer_ WGLBlend.Blend
    | HtmlLayer HtmlLayer_ HtmlBlend.Blend


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
    , now : TimeNow
    , timeShift : TimeDelta
    , product : Product
    , controlsVisible : Bool
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }

-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> HTML Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend =
    ( Maybe WGLBlend.Blend, Maybe String )


type alias PortModel =
    { background : String
    , layers : List PortLayerDef
    , mode : String
    , mouse : ( Int, Int )
    , now : Float
    , origin : (Int, Int)
    , size : (Int, Int)
    , theta : Float
    , omega : Float
    , product : String
    , palette : List String
    }


type alias PortLayerDef =
    { kind : String
    , blend : PortBlend
    , webglOrHtml : String
    , isOn : Bool
    , name : String
    , model : String
    }


init
    :  UiMode
    -> List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> Model
init uiMode initialLayers createLayer =
    let
        emptyModel = initEmpty uiMode
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
        | gui =
            case uiMode of
                TronUi innerUiMode ->
                    Just <| gui { modelWithLayers | mode = innerUiMode }
                _ -> Nothing
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
    HtmlLayer NoContent HtmlBlend.default


sizePresets : UiMode -> ( Dict.Dict String ( Int, Int ), Shape )
sizePresets mode =
    case mode of
        Production ->
            -- WALLPAPER_SIZES
            ( Dict.fromList
                [ ( "2560x1440", ( 2560, 1440 ) )
                , ( "1920x1200", ( 1920, 1200 ) )
                , ( "1920x1080", ( 1920, 1080 ) )
                , ( "1680x1050", ( 1680, 1050 ) )
                , ( "1536x864", ( 1536, 864 ) )
                , ( "1440x900", ( 1440, 900 ) )
                , ( "1366x768", ( 1366, 768 ) )
                ]
            , ( 4, 2 )
            )
        Ads ->
            -- ADS SIZES 
            ( Dict.fromList
                [( "120x600", ( 120, 600 ) )
                ,( "125x125", ( 125, 125 ) )
                ,( "130x100", ( 130, 100 ) )
                ,( "180x150", ( 180, 150 ) )
                ,( "200x125", ( 200, 125 ) )
                ,( "200x200", ( 200, 200 ) )
                ,( "220x250", ( 220, 250 ) )
                ,( "250x250", ( 250, 250 ) )
                ,( "260x200", ( 260, 200 ) )
                ,( "300x250", ( 300, 250 ) )
                ,( "320x100", ( 320, 100 ) )
                ,( "320x50", ( 320, 50 ) )
                ,( "336x280", ( 336, 280 ) )
                ,( "468x60", ( 468, 60 ) )
                ,( "160x600", ( 160, 600 ) )
                ,( "300x600", ( 300, 600 ) )
                ,( "728x90", ( 728, 90 ) )
                ,( "800x320", ( 800, 320 ) )
                ,( "970x250", ( 970, 250 ) )
                ,( "970x90", ( 970, 90 ) )
                ,( "960x90 baidu", ( 960, 90 ) )
                ,( "728x90 baidu", ( 728, 90 ) )
                ,( "468x60 baidu", ( 468, 60 ) )
                ,( "200x200 baidu", ( 200, 200 ) )
                ,( "960x60 baidu", ( 960, 60 ) )
                ,( "640x60 baidu", ( 640, 60 ) )
                ,( "580x90 baidu", ( 580, 90 ) )
                ,( "460x60 baidu", ( 460, 60 ) )
                ,( "300x250 baidu", ( 300, 250 ) )
                ,( "336x280 baidu", ( 336, 280 ) )
                ,( "1200x628 fb", ( 1200, 628 ) )
                ,( "800x418 tw", ( 800, 418 ) )
                ,( "1080x1080 in", ( 1080, 1080 ) )
                ,( "1200x627 ln", ( 1200, 627 ) )                
                ]
            , ( 4, 2 )
            )   
        _ ->
            -- RELEASE_SIZES // TODO: Multiply for creating @2x @3x
            ( Dict.fromList
                [ ( "480x297 prodcard", ( 480, 297 ) ) -- product card
                , ( "960x594 prodcard@2x", ( 960, 594 ) ) -- @2x product card
                , ( "640x400 spl", ( 640, 400 ) ) -- product splash background
                , ( "1280x800 spl@2x", ( 1280, 800 ) ) -- @2x splash background
                , ( "650x170 nwlt", ( 650, 170 ) ) -- newsletter
                , ( "1300x340 nwlt@2x", ( 1300, 340 ) ) -- @2x newsletter
                , ( "800x418 tw", ( 800, 418 ) ) -- Twitter
                , ( "1200x628 fb", ( 1200, 628 ) ) -- Facebook
                , ( "1280x800 wprev", ( 1280, 800 ) ) -- Webpage Preview
                , ( "800x400 blog", ( 800, 400 ) ) -- Blog
                , ( "1600x800 blog@2x", ( 1600, 800 ) ) -- @2x Blog
                , ( "800x155 bfoot", ( 800, 155 ) ) -- Blog footer
                , ( "1600x310 bfoot", ( 1600, 310 ) ) -- @2x Blog footer
                , ( "2850x1200 landg", ( 2850, 1200 ) ) -- Landing page
                -- , ( "browser", ( 0, 0 ) )
                ]
            , ( 4, 4 ) 
            )


gui : Model -> Gui.Model Msg
gui from =
    let
        ( currentSizePresets, sizePresetsShape ) =
            sizePresets from.mode
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
