module Model exposing
    ( init
    , Model
    , Layer
    , LayerIndex
    , LayerDef
    , LayerModel(..)
    , LayerKind(..)
    , Layer(..)
    , WebGLLayer_(..)
    , SVGLayer_(..)
    , CreateLayer
    , GuiConfig
    , Size
    , Pos
    , PortBlend
    , emptyLayer
    )

import Time exposing (Time)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Product exposing (Product)
import Product
import Layer.FSS as FSS
import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.Template as Template
import Layer.Vignette as Vignette

type alias LayerIndex = Int

type alias Size = (Int, Int)
type alias Pos = (Int, Int)

type alias CreateLayer = LayerKind -> Layer

type LayerKind
    = Lorenz
    | Fractal
    | Template
    | Voronoi
    | Fss
    | MirroredFss
    | Text
    | SvgImage
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
    = TextLayer
    | SvgImageLayer
    | NoContent


type Layer
    = WebGLLayer WebGLLayer_ WGLBlend.Blend
    | SVGLayer SVGLayer_ SVGBlend.Blend


-- `change` is needed since we store a sample layer model
-- to use for any layer in the main model
type alias LayerDef =
    { kind: LayerKind
    , layer: Layer
    , model: LayerModel
    , on: Bool
    }


-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> SVG Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend =
    ( Maybe WGLBlend.Blend, Maybe SVGBlend.PortBlend )


type alias Model =
    { paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , layers : List LayerDef
    , size : Size
    , origin : Pos
    , mouse : (Int, Int)
    , now : Time
    , timeShift : Time
    , product : Product
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }


type alias GuiConfig =
    { product : String
    , palette : List String
    , layers : List
        { kind: String
        , blend : PortBlend
        , webglOrSvg: String
        , on: Bool
        }
    , size : ( Int, Int )
    , facesX : Int
    , facesY : Int
    , lightSpeed: Int
    , vignette: Float
    , amplitude : FSS.AmplitudeChange
    , customSize : Maybe (Int, Int)
    }


init
    :  List ( LayerKind, LayerModel )
    -> CreateLayer
    -> Model
init initialLayers createLayer
    = { paused = False
      , autoRotate = False
      , fps = 0
      , theta = 0.1
      , layers = initialLayers |> List.map
            (\(kind, layerModel) ->
                { kind = kind
                , layer = createLayer kind
                , model = layerModel
                , on = True
                })
      , size = ( 1200, 1200 )
      , origin = ( 0, 0 )
      , mouse = ( 0, 0 )
      , now = 0.0
      , timeShift = 0.0
      --, range = ( 0.8, 1.0 )
      , product = Product.JetBrains
      }


emptyLayer : Layer
emptyLayer =
    SVGLayer NoContent SVGBlend.default
