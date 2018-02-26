module Layer.FSS exposing
    ( Config
    , Mesh
    , SerializedScene
    , makeEntity
    , build
    , init
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL
import WebGL.Settings exposing (Setting)

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
    }


type alias SMesh =
    { geometry : SPlane
    , material : SMaterial
    , position : List Float
    , side : SSide
    }


type alias SerializedScene =
    { lights : List SLight
    , meshes : List SMesh
    }


-- Base logic

init : Config
init = {}


makeEntity : Viewport {} -> Maybe SerializedScene -> List Setting -> Mesh -> WebGL.Entity
makeEntity viewport maybeScene settings mesh =
    let
        lights = maybeScene
            |> Maybe.map (\scene -> scene.lights)
            |> Maybe.withDefault []
    in
        WebGL.entityWith
            settings
            vertexShader
            fragmentShader
            mesh
            (uniforms viewport lights)


-- Mesh


type alias Vertex =
    { aAmbient : Vec4
    , aDiffuse : Vec4
    , aCentroid : Vec3
    , aNormal : Vec3
    , aPosition : Vec3
    , aSide : Float
    }


defaultVertex : Vertex
defaultVertex =
    { aAmbient = vec4 0 0 0 0
    , aCentroid = vec3 0 0 0
    , aDiffuse = vec4 0 0 0 0
    , aNormal = vec3 0 0 0
    , aPosition = vec3 0 0 0
    , aSide = 0
    }


build : Config -> Maybe SerializedScene -> Mesh
build config maybeScene =
    WebGL.triangles
        (maybeScene
            |> Maybe.map (\scene ->
                case List.head scene.meshes of
                    Just mesh ->
                        convertTriangles
                            mesh.material
                            mesh.side
                            mesh.geometry.triangles
                    Nothing -> []
            )
            |> Maybe.withDefault [ ])



convertTriangles : SMaterial -> SSide -> List STriangle -> List ( Vertex, Vertex, Vertex )
convertTriangles material side src =
    src |>
        List.map
            (\sTriangle ->
                case sTriangle.vertices of
                    a::b::c::_ ->
                        ( a |> convertVertex material sTriangle side
                        , b |> convertVertex material sTriangle side
                        , c |> convertVertex material sTriangle side
                        )
                    _ ->
                        ( defaultVertex
                        , defaultVertex
                        , defaultVertex
                        )
            )


convertVertex : SMaterial -> STriangle -> SSide -> SVertex -> Vertex
convertVertex material triangle side v =
    { aSide = side
    , aAmbient = v4fromList material.ambient.rgba
    , aDiffuse = v4fromList material.diffuse.rgba
    , aPosition = v3fromList v.position
    , aCentroid = v3fromList triangle.centroid
    , aNormal = v3fromList triangle.normal
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
        { uResolution : Vec3
        , uLightPosition : Mat4
        , uLightAmbient : Mat4
        , uLightDiffuse : Mat4
        , uResolution : Vec3
        }


uniforms : Viewport {} -> List SLight -> Uniforms
uniforms v lights =
    let
        adaptedLights = lights |> adaptLights
        width = Vec2.getX v.size
        height = Vec2.getY v.size
    in
        -- { perspective = Mat4.mul v.perspective v.camera }
        { uResolution = vec3 width height width

        , uLightAmbient = adaptedLights.ambient
        , uLightDiffuse = adaptedLights.diffuse
        , uLightPosition = adaptedLights.position

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


adaptLights : List SLight -> { ambient : Mat4, diffuse : Mat4, position : Mat4 }
adaptLights srcLights =
    let
        emptyRows =
            { ambient = vec4 0 0 0 0
            , diffuse = vec4 0 0 0 0
            , position = vec3 0 0 0
            }
        lightRows = srcLights |> List.map getRows
    in
        case lightRows of
            a::b::c::d::_ ->
                let
                    ( aa, ba, ca, da ) = ( a.ambient, b.ambient, c.ambient, d.ambient )
                    ( ad, bd, cd, dd ) = ( a.diffuse, b.diffuse, c.diffuse, d.diffuse )
                    ( ap, bp, cp, dp ) = ( a.position, b.position, c.position, d.position )
                in
                    { ambient = Mat4.fromRecord
                        { m11 = Vec4.getX aa, m12 = Vec4.getY aa, m13 = Vec4.getZ aa, m14 = Vec4.getW aa
                        , m21 = Vec4.getX ba, m22 = Vec4.getY ba, m23 = Vec4.getZ ba, m24 = Vec4.getW ba
                        , m31 = Vec4.getX ca, m32 = Vec4.getY ca, m33 = Vec4.getZ ca, m34 = Vec4.getW ca
                        , m41 = Vec4.getX da, m42 = Vec4.getY da, m43 = Vec4.getZ da, m44 = Vec4.getW da
                        }
                    , diffuse = Mat4.fromRecord
                        { m11 = Vec4.getX ad, m12 = Vec4.getY ad, m13 = Vec4.getZ ad, m14 = Vec4.getW ad
                        , m21 = Vec4.getX bd, m22 = Vec4.getY bd, m23 = Vec4.getZ bd, m24 = Vec4.getW bd
                        , m31 = Vec4.getX cd, m32 = Vec4.getY cd, m33 = Vec4.getZ cd, m34 = Vec4.getW cd
                        , m41 = Vec4.getX dd, m42 = Vec4.getY dd, m43 = Vec4.getZ dd, m44 = Vec4.getW dd
                        }
                    , position = Mat4.fromRecord
                        { m11 = Vec3.getX ap, m12 = Vec3.getY ap, m13 = Vec3.getZ ap, m14 = 0
                        , m21 = Vec3.getX bp, m22 = Vec3.getY bp, m23 = Vec3.getZ bp, m24 = 0
                        , m31 = Vec3.getX cp, m32 = Vec3.getY cp, m33 = Vec3.getZ cp, m34 = 0
                        , m41 = Vec3.getX dp, m42 = Vec3.getY dp, m43 = Vec3.getZ dp, m44 = 0
                        }
                    }
            _ ->
                { ambient = Mat4.identity
                , diffuse = Mat4.identity
                , position = Mat4.identity
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

        // Uniforms
        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;

        uniform vec3 uResolution;

        uniform mat4 uLightPosition;
        uniform mat4 uLightAmbient;
        uniform mat4 uLightDiffuse;

        // Varyings
        varying vec4 vColor;

        // Main
        void main() {

            // Create color
            vColor = vec4(0.0);
            //vColor = vec4(0.0, 0.0, 0.0, 1.0);

            // Calculate the vertex position
            //vec3 position = aPosition / uResolution * 2.0;
            vec3 position = aPosition;

            // Iterate through lights
            for (int i = 0; i < 3; i++) {
                vec3 lightPosition = vec3(uLightPosition[i]);
                vec4 lightAmbient = uLightPosition[i];
                vec4 lightDiffuse = uLightPosition[i];

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

                // Calculate ambient light
                vColor += aAmbient * lightAmbient;

                // Calculate diffuse light
                vColor += aDiffuse * lightDiffuse * illuminance;
            }

            // Clamp color
            vColor = clamp(vColor, 0.0, 1.0);
            //vColor = vec4(1.0, 1.0, 1.0, 1.0);

            // Set gl_Position
            //gl_Position = vec4(position, 1.0);
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);

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

        // Main
        void main() {

            // Set gl_FragColor
            gl_FragColor = vColor;

        }

    |]
