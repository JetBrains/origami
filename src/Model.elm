module Model exposing
    ( Model
    , ModelChange
    , Layer
    , LayerIndex
    , LayerDef
    , LayerModel(..)
    , LayerKind(..)
    , WebGLLayer(..)
    , SVGLayer(..)
    , GuiConfig
    , Size
    , Pos
    , PortBlend
    )

import Either exposing (Either)
import Time exposing (Time)

import WebGL.Blend as WGLBlend
import Svg.Blend as SVGBlend

import Product exposing (Product)
import Product
import Layer.Vignette as Vignette
import Layer.FSS as FSS
import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.Template as Template

type alias LayerIndex = Int

type alias Size = (Int, Int)
type alias Pos = (Int, Int)

type alias ModelChange = LayerModel -> LayerModel

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


-- type LayerBlend
--     = WGLB WGLBlend.Blend
--     | SVGB SVGBlend.Blend


type LayerModel
    = LorenzModel Lorenz.Model
    | FractalModel Fractal.Model
    | VoronoiModel Voronoi.Model
    | FssModel FSS.Model
    | TemplateModel Template.Model
    | NoModel


type WebGLLayer
    = LorenzLayer Lorenz.Mesh
    | FractalLayer Fractal.Mesh
    | VoronoiLayer Voronoi.Mesh
    | TemplateLayer Template.Mesh
    | FssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | MirroredFssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | VignetteLayer

type SVGLayer
    = TextLayer
    | SvgImageLayer
    -- | CanvasLayer (\_ -> )

type alias Layer =
    Either
        ( WebGLLayer, WGLBlend.Blend )
        ( SVGLayer, SVGBlend.Blend )

-- `change` is needed since we store a sample layer model
-- to use for any layer in the main model
type alias LayerDef =
    { kind: LayerKind
    , layer: Layer
    , change: ModelChange
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
    , vignette : Vignette.Model
    , fss : FSS.Model
    , lorenz : Lorenz.Model
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }


type alias GuiConfig =
    { product : String
    , palette : List String
    , layers : List
        { kind : String
        , blend : PortBlend
        , webglOrSvg : String
        , on : Bool
        }
    , size : ( Int, Int )
    , facesX : Int
    , facesY : Int
    , lightSpeed : Int
    , vignette : Float
    , amplitude : FSS.AmplitudeChange
    , customSize : Maybe (Int, Int)
    }
