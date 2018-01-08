module Lorenz exposing
    ( Config
    , LorenzMesh
    , init
    , makeEntity
    , build
    )

import Array

import WebGL exposing (Mesh)
import Math.Vector3 as Vec3 exposing (Vec3, vec3, getX, getY, getZ)
import WebGL exposing (Mesh, Shader, Entity)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


thickness : Float
thickness = 0.03


type alias Triangle = ( Vertex, Vertex, Vertex )


type alias LorenzMesh = Mesh Vertex


type alias Config =
    { sigma : Float
    , beta : Float
    , rho : Float
    , step : Float
    , numVertices : Int
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    , cameraTranslate : Mat4
    , cameraRotate : Mat4
    }


init : Config
init =
    { sigma = 10
    , beta = 8 / 3
    , rho = 28
    , step = 0.005
    , numVertices = 2000
    }


makeEntity : LorenzMesh -> Float -> Entity
makeEntity mesh theta =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        ( uniforms theta )


build : Config -> LorenzMesh
build config =
    let
        x0 = 0.1
        y0 = 0
        z0 = 0
        -- vertices = Debug.log "vertices" (List.range 1 numVertices
--        vertices = List.range 1 config.numVertices
--            |> List.foldr (\_ positions ->
--                let
--                    len = List.length positions
--                    maybePrev = (List.drop (len - 1) positions) |> List.head
--                in
--                    case maybePrev of
--                        Just prev -> positions ++ [ prev |> step config ]
--                        Nothing -> [ ( vec3 x0 y0 z0, vec3 x0 y0 z0 ) ]
--            ) []
        vertices = List.range 1 config.numVertices
            |> List.foldr (\_ positions ->
                case positions of
                    [] -> [ vec3 x0 y0 z0 ]
                    prev :: _ -> (prev |> step config) :: positions
            ) []
        scaledVertices = vertices
             |> List.map scaleVertex -- TODO: do it with camera matrix!
             |> Array.fromList
    in
        scaledVertices
            |> Array.indexedMap (calculateNormals scaledVertices)
            |> Array.map sumNormals
            |> Array.map trianglePairAt
            |> flattenTriangles
            |> WebGL.triangles


flattenTriangles : Array.Array ( Triangle, Triangle ) -> List Triangle
flattenTriangles src =
    src |>
        Array.foldl
            (\( firstInPair, secondInPair ) allTriangles ->
                allTriangles ++ [ firstInPair ] ++ [ secondInPair ]) []



step : Config -> Vec3 -> Vec3
step config v =
    let
        ( x, y, z ) = ( getX v, getY v, getZ v )
        σ = config.sigma
        β = config.beta
        ρ = config.rho
        -- δt = config.dt / 1000
        δt = config.step
        δx = σ * (y - x) * δt
        δy = ( x * (ρ - z) - y ) * δt
        δz = ( x * y - β * z ) * δt
    in
        vec3 (x + δx) (y + δy) (z + δz)


scaleVertex : Vec3 -> Vec3
scaleVertex v = vec3 (getX v / 10) (getY v / 10) (getZ v / 100)


sumNormals : ( Vec3, Vec3 ) -> Vec3
sumNormals ( prevNorm, nextNorm ) =
    Vec3.add prevNorm nextNorm |> Vec3.normalize


calculateNormals : Array.Array Vec3 -> Int -> Vec3 -> ( Vec3, Vec3 )
calculateNormals vertices idx v =
    let
        ( prevV, nextV ) = case ( vertices |> Array.get (idx - 1)
                                , vertices |> Array.get (idx + 1)
                                ) of
            ( Just prev, Just next ) -> ( prev, next )
            ( Nothing, Just next ) -> ( v, next )
            ( Just prev, Nothing ) -> ( prev, v )
            _ -> ( v, v )
        ( prevDir, nextDir ) =
            ( Vec3.direction v prevV
            , Vec3.direction nextV v
            )
        ( prevNorm, nextNorm ) =
            ( Vec3.cross prevV prevDir |> Vec3.normalize
            , Vec3.cross nextV nextDir |> Vec3.normalize
            )
    in
        ( prevNorm, nextNorm )


trianglePairAt : Vec3 -> ( Triangle, Triangle )
trianglePairAt v =
    ( ( Vertex (vec3 1 1 1) (vec3 1 0 0)
      , Vertex (vec3 1 1 1) (vec3 0 1 0)
      , Vertex (vec3 1 1 1) (vec3 0 0 1)
      )
    , ( Vertex (vec3 1 1 1) (vec3 1 0 0)
      , Vertex (vec3 1 1 1) (vec3 0 1 0)
      , Vertex (vec3 1 1 1) (vec3 0 0 1)
      )
    )

--    let
--        ( prevX, prevY, prevZ ) = ( getX prevV, getY prevV, getZ prevV )
--        ( x, y, z ) = ( getX v, getY v, getZ v )
--        tw = thickness
--        th = thickness
--        -- first triangle, first vertex
--        t1v1 = vec3 x (y + th / 2) z
--        -- first triangle, second vertex
--        t1v2 = vec3 (x + tw) (y + th / 2) z
--        -- first triangle, third vertex
--        t1v3 = vec3 (x + tw / 2) (y - th / 2) z
--        -- second triangle, first vertex
--        t2v1 = vec3 x (y + th / 2) z
--        -- second triangle, second vertex
--        t2v2 = vec3 (x + tw) (y + th / 2) z
--        -- second triangle, third vertex
--        t2v3 = vec3 (x + tw / 2) (y - th / 2) z
--    in
--        ( ( Vertex t1v1 (vec3 1 0 0)
--          , Vertex t1v2 (vec3 0 1 0)
--          , Vertex t1v3 (vec3 0 0 1)
--          )
--        , ( Vertex t2v1 (vec3 1 0 0)
--          , Vertex t2v2 (vec3 0 1 0)
--          , Vertex t2v2 (vec3 0 0 1)
--          )
--        )


uniforms : Float -> Uniforms
uniforms theta =
    { rotation
        = Mat4.makeRotate (3 * theta) (vec3 0 1 0)
    , perspective
        = Mat4.makePerspective 95 1.5 0.1 3000
    , camera = Mat4.makeLookAt (vec3 1 0.5 -0.8) (vec3 -0.5 0.1 0) (vec3 0 1 0)
    , shade = 0.8
    , cameraTranslate = Mat4.makeTranslate (vec3 (1/3) (1/80) (-1/16))
    , cameraRotate = Mat4.makeRotate (2) (vec3 1 0 0)
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * cameraTranslate * cameraRotate  *  vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]

