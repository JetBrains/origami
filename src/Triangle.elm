module Triangle exposing
    ( TriangleMesh
    , mesh
    , entity
    )

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

-- import AnimationFrame
-- import Html exposing (Html)
-- import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
-- import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings exposing (Setting)


type alias TriangleMesh = Mesh Vertex


-- main : Program Never Time Time
-- main =
--     Html.program
--         { init = ( 0, Cmd.none )
--         , view = view
--         , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
--         , update = (\elapsed currentTime -> ( elapsed + currentTime, Cmd.none ))
--         }


entity : Float -> List Setting -> WebGL.Entity
entity t settings =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        { perspective = perspective (t / 1000) }


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
