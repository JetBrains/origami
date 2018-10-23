module Layer.FSS exposing
    ( Model, PortModel
    , RenderMode(..)
    , Mesh
    , SerializedScene
    , Amplitude, AmplitudeChange
    , Clip
    , makeEntity
    , fromPortModel
    , build
    , emptyMesh
    , defaultMirror
    , defaultAmplitude
    , defaultMouse
    , defaultFaces
    , defaultLightSpeed
    , init
    )


import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, getX, getY, getZ)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL
import WebGL.Settings exposing (Setting)
import Time exposing (Time)


import Viewport exposing (Viewport)



type alias Mouse = ( Int, Int )
type alias Size = ( Int, Int )
type alias Origin = ( Int, Int )
type alias Clip = ( Float, Float )
type alias Mirror = Float
type alias Amplitude = ( Float, Float, Float )
type alias AmplitudeChange = ( Maybe Float, Maybe Float, Maybe Float )
type alias Speed = Float


type alias Mesh = WebGL.Mesh Vertex


type RenderMode
    = Triangles
    | Lines
    | Points


type alias PortModel =
    { now : Time
    , origin : Origin
    , mouse : Mouse
    , renderMode : String
    , amplitude : ( Float, Float, Float )
    , clip : Maybe Clip -- max and min values of X for clipping
    }


type alias Model =
    { now : Time
    , origin : Origin
    , mouse : Mouse
    , renderMode : RenderMode
    , amplitude : ( Float, Float, Float )
    , clip : Maybe Clip -- max and min values of X for clipping
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
defaultAmplitude = ( 0.5, 0.5, 0.2 )


defaultMirror : Float
defaultMirror = 0.5


defaultMouse : Mouse
defaultMouse = ( 0, 0 )


defaultFaces : ( Int, Int )
defaultFaces = ( 17, 17 )


defaultLightSpeed : Int
defaultLightSpeed = 600


init : Model
init =
    { now = 0
    , origin = (0, 0)
    , mouse = defaultMouse
    , renderMode = Triangles
    , amplitude = defaultAmplitude
    , clip = Nothing -- (-1, -1) -- max and min values of X for clipping
    }


fromPortModel : PortModel -> Model
fromPortModel portModel =
    { portModel | renderMode = Triangles }


makeEntity
     : Viewport {}
    -> Model
    -> Maybe SerializedScene
    -> List Setting
    -> Mesh
    -> WebGL.Entity
makeEntity viewport model maybeScene settings mesh =
    let
        { now, mouse, amplitude } = model
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
                viewport
                model
                meshSize
                ( lights, speed ))


-- Mesh


type alias Vertex =
    { aAmbient : Vec4
    , aDiffuse : Vec4
    , aCentroid : Vec3
    , aNormal : Vec3
    , aPosition : Vec3
    , aSide : Float
    , aColor : Vec4
    , aV0 : Vec3
    , aPhi : Float
    }


defaultVertex : Vertex
defaultVertex =
    { aAmbient = vec4 0 0 0 0
    , aCentroid = vec3 0 0 0
    , aDiffuse = vec4 0 0 0 0
    , aNormal = vec3 0 0 0
    , aPosition = vec3 0 0 0
    , aSide = 0
    , aColor = vec4 0 0 0 0
    , aV0 = vec3 0 0 0
    , aPhi = 0
    }


emptyMesh : Mesh
emptyMesh =
    WebGL.triangles []


build : Model -> Maybe SerializedScene -> Mesh
build model maybeScene =
    WebGL.triangles
    -- WebGL.lines
    -- WebGL.points
        (maybeScene
            |> Maybe.map (\scene ->
                case List.head scene.meshes of
                    Just mesh ->
                        let
                            triangle =
                                ( quickVertex (vec3 0 0 0)
                                , quickVertex (vec3 1 1 0)
                                , quickVertex (vec3 1 -1 0)
                                )
                            convertedTriangles  = convertTriangles
                                                      mesh.material
                                                      mesh.side
                                                      mesh.geometry.triangles
                        in
                            -- List.map
                            --     (\(v1, v2, _) -> (v1, v2))
                            --     convertedTriangles
                            -- List.foldl (\(v1, v2, v3) lines ->
                            --     lines ++ [ (v1, v2) ] ++ [ (v2, v3) ]
                            -- ) [] convertedTriangles
                            -- List.foldl (\(v1, v2, v3) lines ->
                            --     lines ++ [ v1 ] ++ [ v2 ] ++ [ v3 ]
                            -- ) [] convertedTriangles
                            convertedTriangles
                            --(Debug.log "triangles" [ triangle ])
                    Nothing -> []
            )
            |> Maybe.withDefault [ ])




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
                        , b |> convertVertex (vec4 b.gradient b.gradient b.gradient 1) material sTriangle side
                        , c |> convertVertex (vec4  c.gradient c.gradient c.gradient 1) material sTriangle side
                        )

                    _ ->
                        ( defaultVertex
                        , defaultVertex
                        , defaultVertex
                        )
            )


convertVertex : Vec4 -> SMaterial -> STriangle -> SSide -> SVertex -> Vertex
convertVertex color material triangle side v =
    { aSide = side
    , aAmbient = v4fromList material.ambient.rgba
    , aDiffuse = v4fromList material.diffuse.rgba
    , aPosition = v3fromList v.position
    , aCentroid = v3fromList triangle.centroid
    , aNormal = v3fromList triangle.normal
    , aColor = color
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


-- Shaders


type alias Uniforms =
    Viewport
        { uLightPosition : Mat4
        , uLightAmbient : Mat4
        , uLightDiffuse : Mat4
        , uLightSpeed : Float
        , uResolution : Vec3
        , uNow : Float
        , uMousePosition: Vec2
        , uAmplitude: Vec3
        , uSegment : Vec3
        , uMirror : Float
        , uClip : Vec2
        , uScale : Vec2
        }


type alias Varyings =
    { vColor : Vec4
    , vPosition : Vec3
    --, vMirror : Vec3
    }


uniforms
     : Viewport {}
    -> Model
    -> ( Int, Int )
    -> ( List SLight, Speed )
    -> Uniforms
uniforms v model meshSize ( lights, speed ) =
    let
        { now, origin, mouse, amplitude } = model
        adaptedLights = lights |> adaptLights meshSize speed
        (meshWidth, meshHeight) = meshSize
        width = Vec2.getX v.size
        height = Vec2.getY v.size
        -- width = Tuple.first size |> toFloat
        -- height = Tuple.second size |> toFloat
        depth = 100.0
        ( mirror, clip ) = case model.clip of
            Just clip -> ( 1.0, clip )
            Nothing -> ( 0.0, (-1, -1) )
        ( amplitudeX, amplitudeY, amplitudeZ ) = amplitude
    in
        -- { perspective = Mat4.mul v.perspective v.camera }
        { uResolution = vec3 width height depth
        , uLightAmbient = adaptedLights.ambient
        , uLightDiffuse = adaptedLights.diffuse
        , uLightPosition = adaptedLights.position
        , uLightSpeed = adaptedLights.speed
        , uNow = now
        , uMousePosition = vec2 (toFloat (Tuple.first mouse)) (toFloat (Tuple.second mouse))
        , uSegment  = vec3 100 100 50
        , uMirror = mirror
        , uClip = vec2 (Tuple.first clip) (Tuple.second clip)
        , uScale = vec2 (toFloat meshWidth / width) (toFloat meshHeight / height)
        , uAmplitude = vec3 amplitudeX amplitudeY amplitudeZ

        , paused = v.paused
        , rotation = v.rotation
        , perspective = v.perspective
        , camera = v.camera
        --, shade = 0.8
        , cameraTranslate = v.cameraTranslate
        , cameraRotate = v.cameraRotate
        , size = v.size
        , origin = v.origin
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

        // Attributes
        attribute float aSide;
        attribute vec3 aPosition;
        attribute vec3 aCentroid;
        attribute vec3 aNormal;
        attribute vec4 aAmbient;
        attribute vec4 aDiffuse;
        attribute vec4 aColor;
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

        uniform mat4 uLightPosition;
        uniform mat4 uLightAmbient;
        uniform mat4 uLightDiffuse;
        uniform float uLightSpeed;

        uniform vec3 uAmplitude;

        uniform vec2 uMousePosition;
        uniform float uMirror;
        uniform vec2 uClip;

        // Varyings
        varying vec4 vColor;
        varying vec3 vPosition;
        //varying vec3 vMirror;




       // Don't ask
       vec2 _m = uMousePosition * vec2(1.0, -1.0) + vec2( -150.0,  (200.0 + uResolution.y / 2.0));
       vec2 mousePosition =  vec2(_m.x - uResolution.x / 2.0, _m.y - uResolution.y / 10.0 ) / uResolution.xy * 2.0;

       float time = uNow;
       float duration = 5000.0;
       vec3 position = vec3(0.0);
       vec3 disturb = vec3(0.0);


       float attenuator(float curTime, float len) {
          if (curTime < len) {
             return curTime / len;
          } else {
             return 1.0;
          }
       }

        vec3 oscillators(vec3 arg) {
            return vec3(sin(arg[0]), cos(arg[1]), sin(arg[2]));
        }


        // Main
        void main() {

            float phase = aPhi;
            vec3 speed = normalize(aV0) * 0.001;

            // Create color
            vColor = vec4(0.0);


            // Calculate the vertex position
            //vec3 amplitudes = vec3(0.5, 0.2, 0.2) * uSegment;
            vec3 amplitudes = uAmplitude * uSegment;

            // Light geometry and magnitudes
            vec3 orbitFactor = vec3(1.0, 1.0, 2.0);
            vec3 lightsSpeed = vec3(uLightSpeed, uLightSpeed, 100.0);
            vec3 brightnessD = vec3(3.5, 2.5, 2.0);
            vec3 brightnessA = vec3(1.0, 1.0, 0.0);


            position = aPosition;
            //position = position;
            //position = vec3(1.0 - position.x, position.y, position.z);

            if (!paused) {
                vec2 dist = mousePosition - position.xy;
                vec2 dir = normalize(dist);
                float r = length(dist);
                r = clamp (r, 1.0, 2.0);
                disturb = vec3( 0.005 * dir / (r * r) , 0.0);
                // disturb /= uResolution * vec3(uScale, 1.0);
            }

            position += disturb;


            position += attenuator (time, duration) * amplitudes * oscillators(speed * time + phase);

            position /= uResolution * vec3(uScale, 1.0);

            position *= 4.0;
            ;



            // Iterate through lights
            for (int i = 0; i < 1; i++) {
            // for (int i = 0; i < 2; i++) {
                vec3 lightPosition = orbitFactor[i] * vec3(uLightPosition[i]) * oscillators(vec3(vec2(uNow / lightsSpeed[i]), 90.0)) ;
                vec4 lightAmbient = brightnessA[i] * uLightAmbient[i];
                vec4 lightDiffuse = brightnessD[i] * uLightDiffuse[i];

                vec3 ray = normalize(lightPosition - aCentroid  + disturb * 2000.0);
                float illuminance = dot(aNormal, ray) ;
                illuminance = max(illuminance, 0.0);

                // Calculate ambient light
                vColor += aAmbient * lightAmbient;

                // Calculate diffuse light
                vColor += aDiffuse  * lightDiffuse * illuminance;

            }


           // Multiplied by gradients
              vColor *= mix(aColor, vColor, abs(position.z) );

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

        // Varyings
        varying vec4 vColor;
        varying vec3 vPosition;
        //varying vec3 vMirror;

        uniform vec3 uResolution;
        uniform float uNow;
        uniform vec2 uClip;



        vec4 bgColor = vec4(0.0, 0.0, 0.0, 1.0);

        float vignette = 1.0;

        float noise(vec2 seed, float time) {
            float x = (seed.x / 3.14159 + 4.0) * (seed.y / 13.0 + 4.0) * ((fract(time) + 1.0) * 10.0);
            return mod((mod(x, 13.0) + 1.0) * (mod(x, 123.0) + 1.0), 0.01) - 0.005;
        }

        float brightness(vec4 color) {
                return (0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b);
        }

        // Main
        void main() {

            vec2 actPos = gl_FragCoord.xy / uResolution.xy;
            // if (actPos.x > 0.6) discard;
            // if ((vMirror.x >= 0.0) && (vMirror.y >= 0.0)) {

            // }

            if ((uClip.x >= 0.0) && (uClip.y >= 0.0)) {
                if ((actPos.x >= uClip.x) && (actPos.x <= uClip.y)) {
                    discard;
                }
            }

            // Set gl_FragColor
            //   gl_FragColor = vColor;

            // noise by brightness
               gl_FragColor = mix(vColor, vec4(noise(actPos * 1000.0, 1.0) * 100.0), 0.016 / pow(brightness(vColor), 0.5));

            // vignette
               gl_FragColor =  mix(gl_FragColor, bgColor, pow(smoothstep(1.0 - vignette, 0.8, distance(actPos,vec2(0.5))), 2.0));



        }

    |]
