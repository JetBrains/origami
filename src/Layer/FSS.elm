module Layer.FSS exposing
    ( Config
    , Mesh
    , SerializedScene
    , makeEntity
    , build
    , init
    )

import Array

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2
import Math.Vector3 as Vec3 exposing (vec3, Vec3, getX, getY, getZ)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL
import WebGL.Settings exposing (Setting)
import Time exposing (Time)

import Viewport exposing (Viewport)


type alias Config = {}


type alias Mesh = WebGL.Mesh Vertex


-- Serialization


type alias SColor =
    { rgba : List Float
    , hex : String
    , opacity : Float
    }


type alias SLight =
    { ambient : SColor
    , diffuse : SColor
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
    , segmentWidth : Int
    , sliceHeight : Int
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
    , step : List Float
    , anchor : List Float
    , time : Float
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

init : Config
init = {}


makeEntity : Viewport {} -> Time -> Maybe SerializedScene -> List Setting -> Mesh -> WebGL.Entity
makeEntity viewport now maybeScene settings mesh =
    let
        lights = maybeScene
            |> Maybe.map (\scene -> scene.lights)
            |> Maybe.withDefault []
        size = maybeScene
            |> Maybe.map (\scene ->
                List.head scene.meshes )
            |> Maybe.map (\maybeMesh ->
                case maybeMesh of
                    Just mesh -> ( mesh.geometry.width,  mesh.geometry.height )
                    Nothing -> (0, 0)
                )
            |> Maybe.withDefault (0, 0)
    in
        WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            mesh
            (uniforms viewport now size lights)


-- Mesh


type alias Vertex =
    { aAmbient : Vec4
    , aDiffuse : Vec4
    , aCentroid : Vec3
    , aNormal : Vec3
    , aPosition : Vec3
    , aSide : Float
    , aColor : Vec3
    , aStep : Vec3
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
    , aColor = vec3 0 0 0
    , aStep = vec3 0 0 0
    , aPhi = 0
    }


build : Config -> Maybe SerializedScene -> Mesh
build config maybeScene =
    WebGL.triangles
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
                            convertedTriangles
                            --(Debug.log "triangles" [ triangle ])
                    Nothing -> []
            )
            |> Maybe.withDefault [ ])



-- flatten : List ( Vertex, Vertex, Vertex ) -> List Vertex
-- flatten triangle =


quickVertex : Vec3 -> Vertex
quickVertex pos =
    { defaultVertex | aPosition = pos, aColor = vec3 1 0 0 }


convertTriangles :  SMaterial -> SSide -> List STriangle -> List ( Vertex, Vertex, Vertex )
convertTriangles material side src =
    src |>
        List.indexedMap
            (\index sTriangle ->
                case sTriangle.vertices of
                    a::b::c::_ ->
                        case index % 2 of
                            0 ->
                                ( a |> convertVertex (vec3 1 0 0) material sTriangle side
                                , b |> convertVertex (vec3 1 0 0) material sTriangle side
                                , c |> convertVertex (vec3 1 0 0) material sTriangle side
                                )
                            1 ->
                                ( a |> convertVertex (vec3 0 1 0) material sTriangle side
                                , b |> convertVertex (vec3 0 1 0) material sTriangle side
                                , c |> convertVertex (vec3 0 1 0) material sTriangle side
                                )
                            _ ->
                                ( defaultVertex
                                , defaultVertex
                                , defaultVertex
                                )
                    _ ->
                        ( defaultVertex
                        , defaultVertex
                        , defaultVertex
                        )
            )


convertVertex : Vec3 -> SMaterial -> STriangle -> SSide -> SVertex -> Vertex
convertVertex color material triangle side v =
    { aSide = side
    , aAmbient = v4fromList material.ambient.rgba
    , aDiffuse = v4fromList material.diffuse.rgba
    , aPosition = v3fromList v.position -- |> adaptPosition size
    , aCentroid = v3fromList triangle.centroid
    , aNormal = v3fromList triangle.normal
    , aColor = color
    , aStep = v3fromList v.step |> Debug.log "aStep"
    , aPhi = v.time
    }


--findNewPosition : State -> SVertex -> Vec3
--findNewPosition { now, segmentWidth, sliceHeight, depth } { time, step } =
--    let
--        speed = 0.001
--        xRange = 0.35
--        yRange = 0.14
--        zRange = 1.0
--        offset = toFloat depth / 2.0
--        stepVec = v3fromList step
--        ox = sin (time + getX stepVec * now * speed)
--        oy = cos (time + getY stepVec * now * speed)
--        oz = sin (time + getZ stepVec * now * speed)
--    in
--        vec3
--          (xRange * toFloat segmentWidth * ox)
--          (yRange * toFloat sliceHeight * oy)
--          (zRange * offset * oz - offset)


adaptPosition : (Int, Int) -> Vec3 -> Vec3
adaptPosition (w, h) src =
    vec3
      (getX src / toFloat w * 2.0)
      (getY src / toFloat h * 2.0)
      (getZ src / toFloat 1)


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
        { uResolution : Vec3
        , uLightPosition : Mat4
        , uLightAmbient : Mat4
        , uLightDiffuse : Mat4
        , uResolution : Vec3
        , uNow : Float
        , uSegment : Vec3
        }


uniforms : Viewport {} -> Time -> (Int, Int) -> List SLight -> Uniforms
uniforms v now size lights =
    let
        adaptedLights = lights |> adaptLights size
        width = Vec2.getX v.size
        height = Vec2.getY v.size
    in
        -- { perspective = Mat4.mul v.perspective v.camera }
        { uResolution = vec3 width height width

        , uLightAmbient = adaptedLights.ambient
        , uLightDiffuse = adaptedLights.diffuse
        , uLightPosition = adaptedLights.position
        , uNow = now
        , uSegment  = vec3 100 100 50

        , rotation = v.rotation
        , perspective = v.perspective
        , camera = v.camera
        --, shade = 0.8
        , cameraTranslate = v.cameraTranslate
        , cameraRotate = v.cameraRotate

        , size = v.size
        }


type alias LRow = { ambient : Vec4, diffuse : Vec4, position : Vec3 }


getRows : SLight -> LRow
getRows light =
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


adaptLights : (Int, Int) -> List SLight -> { ambient : Mat4, diffuse : Mat4, position : Mat4 }
adaptLights size srcLights =
    let
        emptyVec4 = vec4 0 0 0 0
        emptyVec3 = vec3 0 0 0
        emptyRows =
            { ambient = emptyVec4
            , diffuse = emptyVec4
            , position = emptyVec3
            }
        lightRows = srcLights |> List.map getRows
    in
        case lightRows of
            [a] ->
                let
                    rowA = ( a.ambient, emptyVec4, emptyVec4, emptyVec4 )
                    rowB = ( a.diffuse, emptyVec4, emptyVec4, emptyVec4 )
                    rowC = ( a.position, emptyVec3, emptyVec3, emptyVec3 )
                in
                    lightsToMatrices size rowA rowB rowC
            [a,b] ->
                let
                    rowA = ( a.ambient, b.ambient, emptyVec4, emptyVec4 )
                    rowB = ( a.diffuse, b.diffuse, emptyVec4, emptyVec4 )
                    rowC = ( a.position, b.position, emptyVec3, emptyVec3 )
                in
                    lightsToMatrices size rowA rowB rowC
            [a,b,c] ->
                let
                    rowA = ( a.ambient, b.ambient, c.ambient, emptyVec4 )
                    rowB = ( a.diffuse, b.diffuse, c.diffuse, emptyVec4 )
                    rowC = ( a.position, b.position, c.position, emptyVec3 )
                in
                    lightsToMatrices size rowA rowB rowC
            a::b::c::d::_ ->
                let
                    rowA = ( a.ambient, b.ambient, c.ambient, d.ambient )
                    rowB = ( a.diffuse, b.diffuse, c.diffuse, d.diffuse )
                    rowC = ( a.position, b.position, c.position, d.position )
                in
                    lightsToMatrices size rowA rowB rowC
            _ ->
                { ambient = Mat4.identity
                , diffuse = Mat4.identity
                , position = Mat4.identity
                }

lightsToMatrices
    : (Int, Int)
    -> ( Vec4, Vec4, Vec4, Vec4 )
    -> ( Vec4, Vec4, Vec4, Vec4 )
    ->  ( Vec3, Vec3, Vec3, Vec3 )
    ->
    { ambient : Mat4
    , diffuse : Mat4
    , position : Mat4
    }
lightsToMatrices (w, h) ( aa, ba, ca, da ) ( ad, bd, cd, dd ) ( ap, bp, cp, dp ) =
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
    }



vertexShader : WebGL.Shader Vertex Uniforms { vColor : Vec4 }
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
        attribute vec3 aColor;
        attribute vec3 aStep;
        attribute float aPhi;
        

        // Uniforms
        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;

        uniform vec3 uResolution;

        uniform vec3 uSegment;
        uniform float uNow;

        uniform mat4 uLightPosition;
        uniform mat4 uLightAmbient;
        uniform mat4 uLightDiffuse;

        // Varyings
        varying vec4 vColor;


        float rand(vec2 n) { 
            return fract(sin(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);
        }

        float noise(vec2 p){
            vec2 ip = floor(p);
            vec2 u = fract(p);
            u = u*u*(3.0-2.0*u);
            
            float res = mix(
                mix(rand(ip),rand(ip+vec2(1.0,0.0)),u.x),
                mix(rand(ip+vec2(0.0,1.0)),rand(ip+vec2(1.0,1.0)),u.x),u.y);
            return res*res;
        }

        // Main
        void main() {

            // Create color
            vColor = vec4(0.0);

            // Calculate the vertex position
            //vec3 position; // = aPosition / uResolution * 2.0;
            //vec3 position = aPosition;
            //position.z = noise(position.xy);


            float speed = 0.001;
            float xRange = 0.35;
            float yRange = 0.14;
            float zRange = 1.0;
            float offset = uSegment[2] / 2.0;
            float segmentWidth = uSegment[0];
            float sliceHeight = uSegment[1];

            vec3 position = aPosition / uResolution * 2.0;

            //position.x = xRange * segmentWidth * sin(aPhi + aStep[0] * uNow * speed);
            position.x = position.x + (xRange * segmentWidth * sin(aPhi + aStep[0] * uNow * speed));
            position.y = position.y + (yRange * sliceHeight * cos(aPhi + aStep[1] * uNow * speed));
            position.z = position.z + (zRange * offset * sin(aPhi + aStep[2] * uNow * speed) - offset);




            // Iterate through lights
            for (int i = 0; i < 1; i++) {
                vec3 lightPosition = vec3(uLightPosition[i]);
                vec4 lightAmbient = uLightAmbient[i];
                vec4 lightDiffuse = uLightDiffuse[i];

                // Calculate illuminance
                vec3 ray = normalize(lightPosition - aCentroid);
                float illuminance = dot(aNormal, ray);
                if (aSide == 0.0) {
                    illuminance = max(illuminance, 0.0);
                } else if (aSide == 1.0) {
                    illuminance = abs(min(illuminance, 0.0));
                } else if (aSide == 2.0) {
                    illuminance = max(abs(illuminance), 0.0);
                }


            //   vColor = vec4(aColor, 1.0);

                // Calculate ambient light
                vColor += aAmbient * lightAmbient;

                // Calculate diffuse light
                vColor += aDiffuse * lightDiffuse * illuminance;
            }

            // Set gl_Position
            gl_Position = vec4(position, 1.0);
            //gl_Position = perspective * camera * rotation * vec4(position, 1.0);

        }

    |]


-- vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
-- vertexShader =
--     [glsl|

--         attribute vec3 position;
--         attribute vec3 color;

--         uniform mat4 cameraTranslate;
--         uniform mat4 cameraRotate;
--         uniform mat4 perspective;
--         uniform mat4 camera;
--         uniform mat4 rotation;

--         varying vec3 vcolor;

--         void main () {
--             // gl_Position = perspective * camera * rotation * cameraTranslate * cameraRotate * vec4(position, 1.0);
--             // gl_Position = perspective * camera * rotation * vec4(position, 1.0);
--             gl_Position = perspective * camera * rotation * vec4(position, 1.0);
--             vcolor = color;
--         }

--     |]


fragmentShader : WebGL.Shader {} Uniforms { vColor : Vec4 }
fragmentShader =
    [glsl|

        // Precision
        precision mediump float;

        // Varyings
        varying vec4 vColor;

        uniform vec3 uResolution;

        // Main
        void main() {

            // Set gl_FragColor
           gl_FragColor = vColor;

                       vec2 uv = gl_FragCoord.xy / uResolution.xy;

                     //      gl_FragColor = texture2D(uv, vec2(abs(0.5 - uv.x), uv.y));
            // gl_FragColor = texture2D(uv, vec2(abs(0.5 - uv.x), uv.y));
             //gl_FragColor = vec4(uv, floor(uv.x + 0.5), uv.y, 1.0);

          //gl_FragColor = vec4(uv, 0.0, 1.0);

        }

    |]
