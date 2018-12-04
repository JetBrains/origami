module Layer.FSS exposing
    ( Model, PortModel
    , RenderMode(..)
    , Mesh
    , SerializedScene
    , Amplitude, AmplitudeChange
    , ColorShift, ColorShiftPatch
    , Opacity
    , Vignette, Iris
    , Clip
    , makeEntity
    , build
    , emptyMesh
    , defaultMirror
    , noClip
    -- , defaultAmplitude
    -- , defaultFaces
    -- , defaultLightSpeed
    , init
    , decodeRenderMode, encodeRenderMode
    )


import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, getX, getY, getZ)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL
import WebGL.Settings exposing (Setting)
import Time exposing (Time)


import Viewport exposing (Viewport)
import Product exposing (ProductId)




type alias Mouse = ( Int, Int )
type alias Faces = ( Int, Int )
type alias Clip = ( Float, Float )
type alias Mirror = Float
type alias Amplitude = ( Float, Float, Float )
type alias ColorShift = ( Float, Float, Float )
type alias Opacity = Float
type alias AmplitudeChange = ( Maybe Float, Maybe Float, Maybe Float )
type alias ColorShiftPatch = ( Maybe Float, Maybe Float, Maybe Float )
type alias Speed = Float
type alias Vignette = Float
type alias Iris = Float



type alias Mesh = WebGL.Mesh Vertex


type RenderMode
    = Triangles
    | Lines
    | PartialLines
    | Points


type alias PortModel =
    { renderMode : String
    , amplitude : Amplitude
    , colorShift : ColorShift
    , opacity : Opacity
    , vignette : Vignette
    , iris : Iris
    , faces : Faces
    , mirror : Bool
    , clip : Maybe Clip -- max and min values of X for clipping
    , lightSpeed : Int
    , shareMesh : Bool
    }


type alias Model =
    { renderMode : RenderMode
    , amplitude : Amplitude
    , colorShift : ColorShift
    , opacity : Opacity
    , vignette : Vignette
    , iris : Iris
    , faces : Faces
    , mirror : Bool
    , clip : Maybe Clip -- max and min values of X for clipping
    , lightSpeed : Int
    , shareMesh : Bool
    }


type alias SColor =
    { rgba : List Float
    , hex : String
    , opacity : Float
    }


type alias SLight =
    { ambient : SColor
    , diffuse : SColor
    , speed : Speed
    , position : List Float
    , ray : List Float
    }


type alias SMaterial =
    { ambient : SColor
    , diffuse : SColor
    , slave : SColor
    }


type alias SPlane =
    { width : Int
    , height : Int
    , triangles : List STriangle
    , vertices : List SVertex
    , segmentWidth : Float
    , sliceHeight : Float
    }


type alias SSide = Float


type alias STriangle =
    { a : SVertex
    , b : SVertex
    , c : SVertex
    , centroid : List Float
    , color : SColor
    , normal : List Float
    , u : List Float
    , v : List Float
    , vertices : List SVertex
    }


type alias SVertex =
    { position : List Float
    , v0 : List Float
    , anchor : List Float
    , time : Float
    , gradient : Float
    }


type alias SMesh =
    { geometry : SPlane
    , material : SMaterial
    , position : List Float
    , side : SSide
    --, depth : Int
    }


type alias SerializedScene =
    { lights : List SLight
    , meshes : List SMesh
    }


-- Base logic

defaultAmplitude : ( Float, Float, Float )
defaultAmplitude = ( 0.3, 0.3, 0.3 )


defaultColorShift : ( Float, Float, Float )
defaultColorShift = ( 0.0, 0.0, 0.0 )


defaultVignette : Vignette
defaultVignette = 0.0


defaultIris : Iris
defaultIris = 0.07


defaultOpacity : Opacity
defaultOpacity = 1.0


defaultMirror : Float
defaultMirror = 0.50001


defaultFaces : ( Int, Int )
defaultFaces = ( 17, 17 )


defaultLightSpeed : Int
defaultLightSpeed = 1000


noClip : Clip
noClip = ( -1, -1 )


init : Model
init =
    { faces = defaultFaces
    , renderMode = Triangles
    , amplitude = defaultAmplitude
    , colorShift = defaultColorShift
    , opacity = defaultOpacity
    , vignette = defaultVignette
    , iris = defaultIris
    , mirror = False
    , clip = Nothing -- (-1, -1) -- max and min values of X for clipping
    , lightSpeed = defaultLightSpeed
    , shareMesh = False
    }


makeEntity
     : Time
    -> Mouse
    -> Int
    -> ProductId
    -> Viewport {}
    -> Model
    -> Maybe SerializedScene
    -> List Setting
    -> Mesh
    -> WebGL.Entity
makeEntity now mouse productId layerIndex viewport model maybeScene settings mesh =
    let
        lights = maybeScene
            |> Maybe.map (\scene -> scene.lights)
            |> Maybe.withDefault []
        speed = case lights of
            first::_ -> first.speed
            _ -> 0
        meshSize = maybeScene
            |> Maybe.map (\scene ->
                List.head scene.meshes)
            |> Maybe.map (\maybeMesh ->
                case maybeMesh of
                    Just mesh -> ( mesh.geometry.width,  mesh.geometry.height )
                    Nothing -> (0, 0)
                )
            |> Maybe.withDefault (0, 0)
        offset = (0, 0)
    in
        WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            mesh
            (uniforms
                now
                mouse
                productId
                viewport
                model
                meshSize
                ( lights, speed )
                layerIndex
                )


-- Mesh


type alias Vertex =
    { materialAmbient : Vec4
    , materialDiffuse : Vec4
    , aCentroid : Vec3
    , aNormal : Vec3
    , aPosition : Vec3
    , aSide : Float
    , aGradient : Vec4
    , aV0 : Vec3
    , aPhi : Float
    }


defaultVertex : Vertex
defaultVertex =
    { materialAmbient = vec4 0 0 0 0
    , aCentroid = vec3 0 0 0
    , materialDiffuse = vec4 0 0 0 0
    , aNormal = vec3 0 0 0
    , aPosition = vec3 0 0 0
    , aSide = 0
    , aGradient = vec4 0 0 0 0
    , aV0 = vec3 0 0 0
    , aPhi = 0
    }


emptyMesh : Mesh
emptyMesh =
    WebGL.triangles []


build : Model -> Maybe SerializedScene -> Mesh
build model maybeScene =
    let
        convertedTriangles =
            (maybeScene
            |> Maybe.map (\scene ->
                case List.head scene.meshes of
                    Just mesh  ->
                        convertTriangles
                            mesh.material
                            mesh.side
                            mesh.geometry.triangles
                    Nothing -> []
            )
            |> Maybe.withDefault [ ])
    in
        case model.renderMode of
            Triangles -> WebGL.triangles convertedTriangles
            PartialLines ->
                WebGL.lines <|
                    List.map
                        (\(v1, v2, _) -> (v1, v2))
                        convertedTriangles
            Lines ->
                WebGL.lines <|
                    List.foldl (\(v1, v2, v3) lines ->
                            lines ++ [ (v1, v2) ] ++ [ (v2, v3) ]
                        ) [] convertedTriangles
            Points ->
                WebGL.points <|
                    List.foldl (\(v1, v2, v3) lines ->
                        lines ++ [ v1 ] ++ [ v2 ] ++ [ v3 ]
                    ) [] convertedTriangles



quickVertex : Vec3 -> Vertex
quickVertex pos =
    { defaultVertex | aPosition = pos }


convertTriangles :  SMaterial -> SSide -> List STriangle -> List ( Vertex, Vertex, Vertex )
convertTriangles material side src =
    src |>
        List.indexedMap
            (\index sTriangle ->
                case sTriangle.vertices of
                    a::b::c::_ ->
                        ( a |> convertVertex (vec4 a.gradient a.gradient a.gradient 1) material sTriangle side
                        , b |> convertVertex (vec4 a.gradient a.gradient a.gradient 1) material sTriangle side
                        , c |> convertVertex (vec4 b.gradient b.gradient b.gradient 1) material sTriangle side
                        )
                    _ ->
                        ( defaultVertex
                        , defaultVertex
                        , defaultVertex
                        )
            )


convertVertex : Vec4 -> SMaterial -> STriangle -> SSide -> SVertex -> Vertex
convertVertex gradient material triangle side v =
    { aSide = side
    , materialAmbient = v4fromList material.ambient.rgba
    , materialDiffuse = v4fromList material.diffuse.rgba
    , aPosition = v3fromList v.position
    , aCentroid = v3fromList triangle.centroid
    , aNormal = v3fromList triangle.normal
    , aGradient = gradient
    , aV0 = v3fromList v.v0
    , aPhi = v.time
    }


v3fromList : List Float -> Vec3
v3fromList list =
    case list of
        a::b::c::_ -> vec3 a b c
        a::b::_ -> vec3 a b 0
        [a] -> vec3 a 0 0
        _ -> vec3 0 0 0


v4fromList : List Float -> Vec4
v4fromList list =
    case list of
        a::b::c::d::_ -> vec4 a b c d
        a::b::c::_ -> vec4 a b c 0
        a::b::_ -> vec4 a b 0 0
        [a] -> vec4 a 0 0 0
        _ -> vec4 0 0 0 0


encodeRenderMode : RenderMode -> String
encodeRenderMode mode =
    case mode of
        Triangles -> "triangles"
        Lines -> "lines"
        PartialLines -> "partial-lines"
        Points -> "points"


decodeRenderMode : String -> RenderMode
decodeRenderMode str =
    case str of
        "triangles" -> Triangles
        "lines" -> Lines
        "partial-lines" -> PartialLines
        "points" -> Points
        _ -> Triangles


-- Shaders


type alias Uniforms =
    Viewport
        { uLightPosition : Mat4
        , uLightAmbient : Mat4
        , uLightDiffuse : Mat4
        , uLightSpeed : Float
        , uResolution : Vec3
        , uNow : Float
        , uLayerIndex : Int
        , uMousePosition : Vec2
        , uProductId : ProductId
        , uAmplitude : Vec3
        , uColorShift : Vec3
        , uOpacity : Float
        , uVignette : Float
        , uIris : Float
        , uSegment : Vec3
        , uMirror : Float
        , uClip : Vec2
        , uScale : Vec2
        }


type alias Varyings =
    { vColor : Vec4
    , vPosition : Vec3
    , vColor1 : Vec4
    , vColor2 : Vec4
    --, vMirror : Vec3
    }


uniforms
     : Time
    -> Mouse
    -> ProductId
    -> Viewport {}
    -> Model
    -> ( Int, Int )
    -> ( List SLight, Speed )
    -> Int
    -> Uniforms
uniforms now mouse productId v model meshSize ( lights, speed ) layerIndex =
    let
        adaptedLights = lights |> adaptLights meshSize speed
        (meshWidth, meshHeight) = meshSize
        width = Vec2.getX v.size
        height = Vec2.getY v.size
        -- width = Tuple.first size |> toFloat
        -- height = Tuple.second size |> toFloat
        depth = 100.0
        mirror = if model.mirror then 1.0 else 0.0
        clip =  model.clip |> Maybe.withDefault noClip
        ( amplitudeX, amplitudeY, amplitudeZ ) = model.amplitude
        ( hue, saturation, brightness ) = model.colorShift
        opacity = model.opacity

    in
        -- { perspective = Mat4.mul v.perspective v.camera }
        { uResolution = vec3 width height depth
        , uLightAmbient = adaptedLights.ambient
        , uLightDiffuse = adaptedLights.diffuse
        , uLightPosition = adaptedLights.position
        , uLightSpeed = adaptedLights.speed
        , uNow = now
        , uLayerIndex = layerIndex
        , uMousePosition = vec2 (toFloat (Tuple.first mouse)) (toFloat (Tuple.second mouse))
        , uSegment  = vec3 100 100 50
        , uMirror = mirror
        , uClip = vec2 (Tuple.first clip) (Tuple.second clip)
        , uScale = vec2 (toFloat meshWidth / width) (toFloat meshHeight / height)
        , uAmplitude = vec3 amplitudeX amplitudeY amplitudeZ
        , uColorShift = vec3 hue saturation brightness
        , uOpacity = model.opacity
        , uVignette = model.vignette
        , uIris = model.iris
        , paused = v.paused
        , rotation = v.rotation
        , perspective = v.perspective
        , camera = v.camera
        --, shade = 0.8
        , cameraTranslate = v.cameraTranslate
        , cameraRotate = v.cameraRotate
        , size = v.size
        , origin = v.origin
        , uProductId = productId
        }


getLightRows : SLight -> { ambient : Vec4, diffuse : Vec4, position : Vec3 }
getLightRows light =
    { ambient =
        case light.ambient.rgba of
            r::g::b::a::_ -> vec4 r g b a
            _ -> vec4 0 0 0 0
    , diffuse =
        case light.diffuse.rgba of
            r::g::b::a::_ -> vec4 r g b a
            _ -> vec4 0 0 0 0
    , position =
        case light.position of
            x::y::z::_ -> vec3 x y z
            _ -> vec3 0 0 0
    }


adaptLights : (Int, Int) -> Speed -> List SLight -> { ambient : Mat4, diffuse : Mat4, position : Mat4, speed : Speed }
adaptLights size speed srcLights =
    let
        emptyVec4 = vec4 0 0 0 0
        emptyVec3 = vec3 0 0 0
        emptyRows =
            { ambient = emptyVec4
            , diffuse = emptyVec4
            , position = emptyVec3
            }
        lightRows = srcLights |> List.map getLightRows
    in
        case lightRows of
            [a] ->
                let
                    rowA = ( a.ambient, emptyVec4, emptyVec4, emptyVec4 )
                    rowB = ( a.diffuse, emptyVec4, emptyVec4, emptyVec4 )
                    rowC = ( a.position, emptyVec3, emptyVec3, emptyVec3 )
                in
                    lightsToMatrices size speed rowA rowB rowC
            [a,b] ->
                let
                    rowA = ( a.ambient, b.ambient, emptyVec4, emptyVec4 )
                    rowB = ( a.diffuse, b.diffuse, emptyVec4, emptyVec4 )
                    rowC = ( a.position, b.position, emptyVec3, emptyVec3 )
                in
                    lightsToMatrices size speed rowA rowB rowC
            [a,b,c] ->
                let
                    rowA = ( a.ambient, b.ambient, c.ambient, emptyVec4 )
                    rowB = ( a.diffuse, b.diffuse, c.diffuse, emptyVec4 )
                    rowC = ( a.position, b.position, c.position, emptyVec3 )
                in
                    lightsToMatrices size speed rowA rowB rowC
            a::b::c::d::_ ->
                let
                    rowA = ( a.ambient, b.ambient, c.ambient, d.ambient )
                    rowB = ( a.diffuse, b.diffuse, c.diffuse, d.diffuse )
                    rowC = ( a.position, b.position, c.position, d.position )
                in
                    lightsToMatrices size speed rowA rowB rowC
            _ ->
                { ambient = Mat4.identity
                , diffuse = Mat4.identity
                , position = Mat4.identity
                , speed = speed
                }

lightsToMatrices
    : (Int, Int)
    -> Speed
    -> ( Vec4, Vec4, Vec4, Vec4 )
    -> ( Vec4, Vec4, Vec4, Vec4 )
    ->  ( Vec3, Vec3, Vec3, Vec3 )
    ->
    { ambient : Mat4
    , diffuse : Mat4
    , position : Mat4
    , speed : Speed
    }
lightsToMatrices (w, h) speed ( aa, ba, ca, da ) ( ad, bd, cd, dd ) ( ap, bp, cp, dp ) =
    { ambient = Mat4.fromRecord
        { m11 = Vec4.getX aa, m12 = Vec4.getY aa, m13 = Vec4.getZ aa, m14 = Vec4.getW aa
        , m21 = Vec4.getX ba, m22 = Vec4.getY ba, m23 = Vec4.getZ ba, m24 = Vec4.getW ba
        , m31 = Vec4.getX ca, m32 = Vec4.getY ca, m33 = Vec4.getZ ca, m34 = Vec4.getW ca
        , m41 = Vec4.getX da, m42 = Vec4.getY da, m43 = Vec4.getZ da, m44 = Vec4.getW da
        } |> Mat4.transpose
    , diffuse = Mat4.fromRecord
        { m11 = Vec4.getX ad, m12 = Vec4.getY ad, m13 = Vec4.getZ ad, m14 = Vec4.getW ad
        , m21 = Vec4.getX bd, m22 = Vec4.getY bd, m23 = Vec4.getZ bd, m24 = Vec4.getW bd
        , m31 = Vec4.getX cd, m32 = Vec4.getY cd, m33 = Vec4.getZ cd, m34 = Vec4.getW cd
        , m41 = Vec4.getX dd, m42 = Vec4.getY dd, m43 = Vec4.getZ dd, m44 = Vec4.getW dd
        } |> Mat4.transpose
    , position = Mat4.fromRecord
        { m11 = Vec3.getX ap, m12 = Vec3.getY ap, m13 = Vec3.getZ ap, m14 = 0
        , m21 = Vec3.getX bp, m22 = Vec3.getY bp, m23 = Vec3.getZ bp, m24 = 0
        , m31 = Vec3.getX cp, m32 = Vec3.getY cp, m33 = Vec3.getZ cp, m34 = 0
        , m41 = Vec3.getX dp, m42 = Vec3.getY dp, m43 = Vec3.getZ dp, m44 = 0
        } |> Mat4.transpose
    , speed = speed
    }



vertexShader : WebGL.Shader Vertex Uniforms Varyings
vertexShader =
     [glsl|


        // Precision
        precision mediump float;
        precision mediump int;

        // Attributes
        attribute float aSide;
        attribute vec3 aPosition;
        attribute vec3 aCentroid;
        attribute vec3 aNormal;
        attribute vec4 materialAmbient;
        attribute vec4 materialDiffuse;
        attribute vec4 aGradient;
        attribute vec3 aV0;

        attribute float aPhi;


        // Uniforms
        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;


        uniform vec3 uResolution;
        uniform vec2 uScale;

        uniform vec3 uSegment;
        uniform bool paused;
        uniform float uNow;
        uniform int uLayerIndex;

        uniform mat4 uLightPosition;
        uniform mat4 uLightAmbient;
        uniform mat4 uLightDiffuse;
        uniform float uLightSpeed;

        uniform vec3 uAmplitude;
        uniform vec3 uColorShift;
        uniform float uOpacity;

        uniform vec2 uMousePosition;
        uniform int uProductId;
        uniform float uMirror;
        uniform vec2 uClip;

        // Varyings
        varying vec4 vColor;
        varying vec4 vColor1;
        varying vec4 vColor2;
        varying vec3 vPosition;
        //varying vec3 vMirror;


       float time = uNow;
       vec3 position = vec3(0.0);
       bool background = false;
       bool midLayer = false;

       //       vec3 vertexOscillators(vec3 arg) {
       //     return vec3(sin(arg[0]), cos(arg[1]), sin(arg[2]));
       // }



       vec3 vertexOscillators(vec3 arg) {
            return vec3(sin(arg[0]), cos(arg[1]), sin(arg[2]));
        }

        vec3 lightOscillators(vec3 arg) {
           // return vec3( 2.0 * sin( 5.0 * arg[0]), sin( 6.0 * arg[1]), sin(arg[2]));

             return vec3(
                 cos(3.0 * arg[0]),
                 sin(2.0 * arg[1]),
                  sin(arg[2])
                  );


        }

        vec3 rgb2hsv(vec3 c){
                vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
                vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
                vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

                float d = q.x - min(q.w, q.y);
                float e = 1.0e-10;
                return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
            }

        vec3 hsv2rgb(vec3 c){
            vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
            vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
            return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
        }

        vec4 adjustLight(vec4 origColor, vec3 hsb) {
            vec3 changedColor  = rgb2hsv(origColor.rgb);
            changedColor[0] = clamp(changedColor[0] + hsb[0], 0.0, 1.0); // hue shift
            changedColor[1] = clamp(changedColor[1] + hsb[1], 0.0, 1.0); // saturation shift
            changedColor[2] = clamp(changedColor[2] + hsb[2], 0.0, 1.0); // brightness shift
            return vec4(vec3(hsv2rgb(changedColor)), 1.0);
        }


        // Main
        void main() {

           if ( uLayerIndex == 0 ) {

               background = true;

           }

            if ( uLayerIndex == 1 ) {
              midLayer = true;
           }


            float phase = aPhi;
            vec3 speed = normalize(aV0) * 0.0007;

            // Create color
             vColor1 = materialAmbient;
             vColor2 = materialDiffuse;
             vColor = vec4(1.0);

            // Calculate the vertex position
            vec3 amplitudes = uAmplitude * uSegment;

            // Light geometry and magnitudes
            vec3 orbitFactor = !background ? vec3(2.5, 2.5, 2.5) : vec3(-2.5, 1.5, 1.2) ;
            vec3 lightsSpeed = !background ? vec3(uLightSpeed * 40.0, uLightSpeed  * 10.0, 100.0) : vec3(uLightSpeed, uLightSpeed  * 5.0, 150.0) ;



            position = aPosition;
            position += amplitudes * vertexOscillators(speed * time + phase);
            position /= uResolution * vec3(uScale, 1.0);
            position *= 4.0;

            vec4 light1 = vec4(0.0);
            vec4 light2 = vec4(0.0);
            vec3 deltaHSVLight1 = vec3(0.0);
            vec3 deltaHSVLight2 = vec3(0.0);
            float shine = 1.0;
            
            // hue shift in shadows 
            vec3 shadowColor = rgb2hsv( aGradient.rgb );


            if( uProductId == 0 ) {
                shadowColor[0] -= 0.25;
                shadowColor[1] = 1.0;
                shadowColor[2] *= 0.5;
            }

            if( uProductId == 1 ) {
                deltaHSVLight1 = vec3( 0.0, 0.0, -0.2 );
                deltaHSVLight2 = vec3( -0.1, 1.0, 1.0 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.9;
                shadowColor[2] *= 0.9;
                shine = 1.1;
                orbitFactor[1] += 2.5;
            }

            if( uProductId == 2 ) {
                deltaHSVLight1 = vec3( 0.1, 0.0, 0.2 );
                deltaHSVLight2 = vec3( -0.1, 0.0, -0.4 );
                shadowColor[0] -= 0.4;
                shadowColor[1] = 0.9;
                shadowColor[2] *= 0.4;
                orbitFactor[2] -= 0.5;
            }    

            if( uProductId == 3 ) {
                deltaHSVLight1 = vec3( 0.1, 1.0, 0.2 );
                deltaHSVLight2 = vec3( -0.1, 1.0, 0.4 );
                shadowColor[0] += 0.3;
                shadowColor[1] = 0.3;
                shadowColor[2] *= 0.4;
                shine = 1.1;
                orbitFactor[1] += 1.5;
            }    

            if( uProductId == 5 ) {
                deltaHSVLight1 = vec3( 0.0, 0.5, -0.1 );
                deltaHSVLight2 = vec3( 0.1, 1.0, 0.7 );
                shadowColor[0] -= 0.1;
                shadowColor[1] = 0.5;
                shadowColor[2] *= 0.5;
                shine = 1.1;
                orbitFactor[1] += 1.5;
            }    
            if( uProductId == 6 ) {
                deltaHSVLight1 = vec3( -0.04, 1.0, -0.1 );
                deltaHSVLight2 = vec3( 0.0, 1.0, 0.9 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.9;
                shadowColor[2] *= 0.5;
                shine = 1.3;
                orbitFactor[0] += 0.5;
            }    
            if( uProductId == 7 ) {
                deltaHSVLight1 = vec3( -0.18, 0.9, 0.8 );
                deltaHSVLight2 = vec3( 0.0, 0.4, 0.8 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.4;
                shadowColor[2] *= 0.4;
                shine = 1.4;
                orbitFactor[0] -= 1.5;
            }  
            if( uProductId == 8 ) {
                deltaHSVLight1 = vec3( -0.03, 0.9, 0.1 );
                deltaHSVLight2 = vec3( 0.0, 0.9, -0.1 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.9;
                shadowColor[2] *= 0.2;
                orbitFactor[1] -= 1.5;
            }  

            if( uProductId == 9 ) {
                deltaHSVLight1 = vec3( 0.1, 0.8, 0.9 );
                deltaHSVLight2 = vec3( 0.0, 0.6, 0.9 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.8;
                shadowColor[2] *= 0.4;
                orbitFactor[1] += 1.5;
            }  
            if( uProductId == 10 ) {
                deltaHSVLight1 = vec3( -0.01, 0.8, -0.1 );
                deltaHSVLight2 = vec3( 0.0, 0.6, 1.0 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.8;
                shadowColor[2] *= 0.5;
                orbitFactor[1] -= 2.0;
            }  
            if( uProductId == 11 ) {
                deltaHSVLight1 = vec3( -0.02, 0.8, -0.1 );
                deltaHSVLight2 = vec3( 0.0, 0.6, 1.0 );
                shadowColor[0] -= 0.3;
                shadowColor[1] = 0.9;
                shadowColor[2] *= 0.3;
                orbitFactor[1] -= 2.0;
            } 

            if( uProductId == 12 ) {
                deltaHSVLight1 = vec3( 0.25, 1.0, 0.5 );
                deltaHSVLight2 = vec3( -0.64, 1.0, 1.0 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 1.0;
                shadowColor[2] *= 0.7;
                orbitFactor[2] += 1.0;
            }
            if( uProductId == 13 ) {
                deltaHSVLight1 = vec3( -0.45, 0.4, 0.9 );
                deltaHSVLight2 = vec3( -0.2, -0.8, 0.7 );
                shadowColor[0] -= 0.2;
                shadowColor[1] = 0.8;
                shadowColor[2] *= 0.4;
                orbitFactor[2] += 1.0;
            }                          
            // Iterate through lights

            for (int i = 0; i < 2; i++) {

          //  if(uLayerIndex != 0) {
                vec3 lightPosition = orbitFactor[i] * vec3(uLightPosition[i]) * lightOscillators(vec3(vec2(uNow / lightsSpeed[i]), 90.0)) ;

                if ( background ) {
                    light1 =  adjustLight(uLightDiffuse[i], uColorShift);
                    light2 =  adjustLight(uLightAmbient[i], uColorShift);
                } else {
                    light1 =  adjustLight(uLightAmbient[i], deltaHSVLight1 + uColorShift);
                    light2 =  adjustLight(uLightDiffuse[i], deltaHSVLight2 + uColorShift);
                }

                vec3 ray = normalize(lightPosition - aCentroid);
                float illuminance = dot(aNormal, ray);
                illuminance = shine * max(illuminance, 0.0);


                // Calculate ambient light
                  vColor *=  light1;

                // Calculate diffuse light
                  vColor +=  light2 * illuminance;

            }

       //Chaotic shadows

             vColor *=  mix(vec4(hsv2rgb(shadowColor), 1.0), vColor, abs(position.z));

           // Set gl_Position
             gl_Position = cameraRotate * cameraTranslate * vec4(position, 1.0);


          if (uMirror > 0.0) {
              gl_Position.x = -1.0 * gl_Position.x;
          }

        }

   |]




fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

        // Precision
        precision mediump float;
        precision mediump int;

        // Varyings
        varying vec4 vColor;
        varying vec4 vColor1;
        varying vec4 vColor2;
        varying vec3 vPosition;
        //varying vec3 vMirror;

        uniform vec3 uResolution;
        uniform float uNow;
        uniform vec2 uClip;
        uniform vec2 uScale;
        uniform float uVignette;
        uniform float uIris;
        uniform float uOpacity;
        uniform int uLayerIndex;


       // vec4 bgColor = vec4(0.0, 0.0, 0.0, 1.0);

       bool midLayer = false;


        float noise(vec2 seed, float time) {
            float x = (seed.x / 3.14159 + 4.0) * (seed.y / 13.0 + 4.0) * ((fract(time) + 1.0) * 10.0);
            return mod((mod(x, 13.0) + 1.0) * (mod(x, 123.0) + 1.0), 0.01) - 0.005;
        }

        float brightness(vec3 color) {
                return (0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b);
        }

        vec3 rgb2hsv(vec3 c){
        vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
        vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
        vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

        float d = q.x - min(q.w, q.y);
        float e = 1.0e-10;
        return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
        }

        vec3 hsv2rgb(vec3 c)
        {
        vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
        vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
        return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
        }

        vec4 adjustColor(vec4 origColor, float deltaHue, float deltaSaturation, float deltaBrightness) {

        vec3 changedColor  = rgb2hsv(origColor.rgb);
        changedColor[0] = mod(changedColor[0] + deltaHue, 1.0); // hue shift
        changedColor[1] = clamp(changedColor[1] + deltaSaturation, 0.0, 1.0); // saturation shift
        changedColor[2] = clamp(changedColor[2] + deltaBrightness, 0.0, 1.0); // brightness shift

        return vec4(vec3(hsv2rgb(changedColor)), 1.0);
        }



        // Main
        void main() {

        if ( uLayerIndex == 1 ) {
              midLayer = true;
           }


            vec2 actPos = gl_FragCoord.xy / uResolution.xy;

            //if (actPos.x < 0.5) discard;
            // if ((vMirror.x >= 0.0) && (vMirror.y >= 0.0)) {

            // }

            // here uClip.y is not Y coord, but upper limit for X
            if ((uClip.x >= 0.0) || (uClip.y >= 0.0)) {
                if ((actPos.x <= uClip.x) || (actPos.x >= uClip.y)) {
                    discard;
                }
            }
             gl_FragColor = vColor;


            // fog
            vec3 shadowHSV = rgb2hsv(vColor1.rgb);
            shadowHSV[2] *=  uVignette + 0.1;
            vec3 shadowRGB = hsv2rgb(shadowHSV);
            gl_FragColor.rgb =  mix(gl_FragColor.rgb, shadowRGB, smoothstep(0.0, 1.3 - uIris, distance(actPos, vec2(0.5))));

            // noise by brightness
            if ( midLayer ) {
               gl_FragColor.rgb = mix(gl_FragColor.rgb, vec3(noise(actPos * 1000.0, 1.0) * 100.0), 0.02 / pow(brightness(gl_FragColor.rgb), 0.1));
            }


            gl_FragColor.a = uOpacity;


             //gl_FragColor.rgb *= gl_FragColor.a;




        }

    |]
