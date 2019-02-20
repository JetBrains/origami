module Model exposing
    ( init
    , initEmpty
    , Model
    , UiMode(..), encodeMode, decodeMode, tryDecodingMode
    , Layer(..)
    , emptyLayer
    , LayerIndex
    , LayerDef
    , LayerModel(..)
    , LayerKind(..)
    , WebGLLayer_(..)
    , HtmlLayer_(..)
    , CreateLayer
    , CreateGui
    , ViewportSize(..)
    , Size
    , Pos
    , TimeDelta
    , PortModel
    , PortLayerDef
    , PortBlend
    , Msg(..)
    , Constants
    , makeConstants
    , SizeRule(..), encodeSizeRule, decodeSizeRule
    , getPresetLabel, getPresetSize, getRuleSize, getRuleSizeOrZeroes
    , getSizePresets
    , SizePresetCode, encodePreset, decodePreset
    )


import Dict as Dict
import Time as Time
import Array as Array
import Browser.Dom exposing (Viewport)

import WebGL.Blend as WGLBlend
import Html.Blend as HtmlBlend

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
type alias CreateGui = Model -> Gui.Model Msg


type SizeRule
    = FromPreset SizePreset
    | UseViewport ViewportSize
    | Custom Int Int
    | Dimensionless


type Msg
    = Bang
    | ChangeMode UiMode
    | ChangeModeAndResize UiMode SizeRule
    | Animate TimeDelta
    | GuiMessage (Gui.Msg Msg)
    | Resize SizeRule
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


type Factor
    = Single
    | Double


type SizePreset
    = ProductCard Factor
    | ProductSplash Factor
    | Newsletter Factor
    | Twitter
    | Facebook
    | Baidu Int Int
    | Ad Int Int
    | Instagram
    | LinkedIn
    | WebpagePreview
    | BlogHeader Factor
    | BlogFooter Factor
    | LandingPage
    | Wallpaper Int Int
    | Unknown -- FIXME: do we need it?


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
    , size : SizeRule
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
    , sizeRule : Maybe String
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


type alias SizePresetCode = String


type alias Constants =
    { sizes : List
        { mode: String
        , values : List
            { label: String
            , width: Int, height: Int
            , code: SizePresetCode
            }
        }
    , products : List { product : String, label: String }
    }


init
    :  UiMode
    -> List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> CreateGui
    -> Model
init uiMode initialLayers createLayer createGui =
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
                    Just <| createGui { modelWithLayers | mode = innerUiMode }
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
    , size = Dimensionless
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


getSizePresets : UiMode -> List SizePreset
getSizePresets mode =
    case mode of
        Release ->
            [ ProductCard Single
            , ProductCard Double
            , ProductSplash Single
            , ProductSplash Double
            , Newsletter Single
            , Newsletter Double
            , Twitter
            , Facebook
            , WebpagePreview
            , BlogHeader Single
            , BlogHeader Double
            , BlogFooter Single
            , BlogFooter Double
            , LandingPage
            ]
        Ads ->
            (
                [ ( 120, 600 )
                , ( 125, 125 )
                , ( 130, 100 )
                , ( 180, 150 )
                , ( 200, 125 )
                , ( 200, 200 )
                , ( 220, 250 )
                , ( 250, 250 )
                , ( 260, 200 )
                , ( 300, 250 )
                , ( 320, 100 )
                , ( 320, 50 )
                , ( 336, 280 )
                , ( 468, 60 )
                , ( 160, 60 )
                , ( 300, 600 )
                , ( 728, 90 )
                , ( 800, 320 )
                , ( 300, 600 )
                , ( 970, 250 )
                , ( 970, 90 )
                ] |> List.map (\(w, h) -> Ad w h)
            ) ++ (
                [ ( 960, 60 )
                , ( 728, 90 )
                , ( 468, 60 )
                , ( 200, 200 )
                , ( 960, 60 )
                , ( 640, 60 )
                , ( 580, 90 )
                , ( 460, 60 )
                , ( 300, 250 )
                , ( 336, 280 )
                ] |> List.map (\(w, h) -> Baidu w h)
            ) ++ (
                [ Facebook
                , Twitter
                , Instagram
                , LinkedIn
                ]
            )
        TronUi tronMode -> getSizePresets tronMode
        _ -> -- return Wallpaper size preset for eveything else
            [ ( 2560, 1440 )
            , ( 1920, 1200 )
            , ( 1920, 1080 )
            , ( 1680, 1050 )
            , ( 1536, 864)
            , ( 1440, 900 )
            , ( 1366, 768 )
            ] |> List.map (\(w, h) -> Wallpaper w h)


getRuleSize : SizeRule -> Maybe ( Int, Int )
getRuleSize rule =
    case rule of
        FromPreset preset -> Just <| getPresetSize preset
        UseViewport (ViewportSize w h) -> Just (w, h)
        Custom w h -> Just (w, h)
        Dimensionless -> Nothing


getRuleSizeOrZeroes : SizeRule -> ( Int, Int )
getRuleSizeOrZeroes rule =
    getRuleSize rule |> Maybe.withDefault (0, 0)


getPresetSize : SizePreset -> ( Int, Int )
getPresetSize preset =
    let
        applyFactor factor ( w, h ) =
            case factor of
                Single -> ( w, h )
                Double -> ( w * 2, h * 2 )
    in
        case preset of
            ProductCard factor -> ( 480, 297 ) |> applyFactor factor
            ProductSplash factor -> ( 640, 400 ) |> applyFactor factor
            Newsletter factor -> ( 650, 170 ) |> applyFactor factor
            Twitter -> ( 800, 418 )
            Facebook -> ( 1200, 628 )
            WebpagePreview -> ( 1200, 800 )
            BlogHeader factor -> ( 800, 400 ) |> applyFactor factor
            BlogFooter factor -> ( 800, 155 ) |> applyFactor factor
            LandingPage -> ( 2850, 1200 )
            Instagram -> ( 1080, 1080 )
            LinkedIn -> ( 1200, 627 )
            Baidu w h -> ( w, h )
            Ad w h -> ( w, h )
            Wallpaper w h -> ( w, h )
            Unknown -> ( -1, -1 )


getPresetLabel : SizePreset -> String
getPresetLabel preset =
    let
        sizeStr = getPresetSize preset |>
            \(w, h) -> String.fromInt w ++ "x" ++ String.fromInt h
        factorStr factor =
            case factor of
                Single -> ""
                Double -> "@2x"
    in
        sizeStr ++ case preset of
            ProductCard factor -> " prodcard" ++ factorStr factor
            ProductSplash factor -> " spl" ++ factorStr factor
            Newsletter factor -> " nwlt" ++ factorStr factor
            Twitter -> " tw"
            Facebook -> " fb"
            WebpagePreview -> " wprev"
            BlogHeader factor -> " blog" ++ factorStr factor
            BlogFooter factor -> " bfoot" ++ factorStr factor
            LandingPage -> " landg"
            Instagram -> " in"
            LinkedIn -> " ln"
            Baidu w h -> " baidu"
            Ad w h -> ""
            Wallpaper w h -> ""
            Unknown -> "unk"


encodePreset : SizePreset -> SizePresetCode
encodePreset preset =
   let
        sizeStr w h = String.fromInt w ++ ":" ++ String.fromInt h
        factorStr factor =
            case factor of
                Single -> "1"
                Double -> "2"
    in
        case preset of
            ProductCard factor -> "PC:" ++ factorStr factor
            ProductSplash factor -> "SP:" ++ factorStr factor
            Newsletter factor -> "NL:" ++ factorStr factor
            Twitter -> "TW"
            Facebook -> "FB"
            WebpagePreview -> "WB"
            BlogHeader factor -> "BH:" ++ factorStr factor
            BlogFooter factor -> "BF:" ++ factorStr factor
            LandingPage -> "LP"
            Instagram -> "IN"
            LinkedIn -> "LN"
            Baidu w h -> "BA:_:" ++ sizeStr w h
            Ad w h -> "AD:_:" ++ sizeStr w h
            Wallpaper w h -> "WP:_:" ++ sizeStr w h
            Unknown -> "UK"


decodePreset : SizePresetCode -> Maybe SizePreset
decodePreset presetStr =
    let
        parts = String.split ":" presetStr
                    |> Array.fromList
        decodeFactor nStr =
            case nStr of
                "1" -> Just Single
                "2" -> Just Double
                _ -> Nothing
        withFactor f =
            Array.get 1 parts
                |> Maybe.andThen decodeFactor
                |> Maybe.map f
        withSize f =
            case ( Array.get 2 parts, Array.get 3 parts ) of
                ( Just wStr, Just hStr ) ->
                    case ( String.toInt wStr, String.toInt hStr ) of
                        ( Just w, Just h ) -> f w h |> Just
                        _ -> Nothing
                _ -> Nothing
        decodeByCode code =
            case code of
                "PC" -> withFactor ProductCard
                "SP" -> withFactor ProductSplash
                "NL" -> withFactor Newsletter
                "TW" -> Just Twitter
                "FB" -> Just Facebook
                "WB" -> Just WebpagePreview
                "BH" -> withFactor BlogHeader
                "BF" -> withFactor BlogFooter
                "LP" -> Just LandingPage
                "IN" -> Just Instagram
                "LN" -> Just LandingPage
                "BA" -> withSize Baidu
                "AD" -> withSize Ad
                "WP" -> withSize Wallpaper
                _ -> Nothing
    in
        Array.get 0 parts
            |> Maybe.andThen decodeByCode


makeConstants : Constants
makeConstants =
    let
        getModeLabel = encodeMode
        sizePresetsConstants mode =
            getSizePresets mode
                |> List.map (\preset ->
                        case getPresetSize preset of
                            ( w, h ) ->
                                { label = getPresetLabel preset
                                , code = encodePreset preset
                                , width = w, height = h
                                }
                    )
    in
        { sizes =
            [ Production, Release, Development, Ads ]
                |> List.map (\mode ->
                      { mode = getModeLabel mode
                      , values = sizePresetsConstants mode
                      }
                   )
        , products = []
        }


encodeMode : UiMode -> String
encodeMode mode =
    case mode of
        Development -> "dev"
        Production -> "prod"
        Release -> "release"
        Ads -> "ads"
        TronUi innerMode -> "tron-" ++ encodeMode innerMode


decodeMode : String -> UiMode
decodeMode mode =
    if String.startsWith "tron-" mode
    then TronUi <| decodeMode <| String.dropLeft 5 mode
    else
        case mode of
            "dev" -> Development
            "prod" -> Production
            "release" -> Release
            "ads" -> Ads
            "tron" -> TronUi Production
            _ -> Production


tryDecodingMode : String -> Result String UiMode
tryDecodingMode mode =
    if String.startsWith "tron-" mode
    then Ok <| TronUi <| decodeMode <| String.dropLeft 5 mode
    else
        case mode of
            "dev" -> Ok Development
            "prod" -> Ok Production
            "release" -> Ok Release
            "ads" -> Ok Ads
            "tron" -> Ok <| TronUi Production
            _ -> Err mode


encodeSizeRule : SizeRule -> String
encodeSizeRule rule =
    case rule of
        Custom w h -> "custom|" ++ String.fromInt w ++ ":" ++ String.fromInt h
        FromPreset preset -> "preset|" ++ encodePreset preset
        UseViewport (ViewportSize w h) -> "viewport|" ++ String.fromInt w ++ ":" ++ String.fromInt h
        Dimensionless -> "dimensionless"


decodeSizeRule : String -> SizeRule
decodeSizeRule str =
    let
        decodeSize f w_and_h defaultWidth defaultHeight =
            case String.split ":" w_and_h of
                wStr::hStr::_ ->
                    case ( String.toInt wStr, String.toInt hStr ) of
                        ( Just w, Just h ) -> f w h
                        _ -> f defaultWidth defaultHeight
                _ -> f defaultWidth defaultHeight
    in case String.split "|" str of
        "custom"::w_and_h::_ ->
            decodeSize Custom w_and_h -1 -1
        "preset"::presetStr::_ ->
            decodePreset presetStr
                |> Maybe.withDefault Unknown
                |> FromPreset
        "viewport"::w_and_h::_ ->
            decodeSize (\w h -> UseViewport (ViewportSize w h)) w_and_h -1 -1
        "dimensionless"::_ -> Dimensionless
        _ -> Dimensionless
