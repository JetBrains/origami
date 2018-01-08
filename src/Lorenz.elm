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


type alias Triangle = ( Vertex, Vertex, Vertex )


type alias LorenzMesh = Mesh Vertex


type alias Config =
    { sigma : Float
    , beta : Float
    , rho : Float
    , step : Float
    , numVertices : Int
    , thickness : Float
    }


type alias WithNormals =
    { index : Int
    , position : Vec3
    , prevPosition : Vec3
    , prevNormal : Vec3
    , prevSumNormal : Maybe Vec3
    , sumNormal : Vec3
    }


type alias Vertex =
    { position : Vec3
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
    , thickness = 1
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
        verticesWithSumNormals = scaledVertices
            |> Array.indexedMap (calculateNormals scaledVertices)
        verticesWithBothSumNormals = verticesWithSumNormals
            |> Array.indexedMap (addPrevSumNormals verticesWithSumNormals)
    in
        verticesWithBothSumNormals
            |> Array.map (trianglePairAt config.thickness)
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


addPrevSumNormals : Array.Array WithNormals -> Int -> WithNormals -> WithNormals
addPrevSumNormals verticesWithNormals idx v =
    let
        maybePrev = verticesWithNormals |> Array.get (idx - 1)
    in
        { v
        | prevSumNormal = maybePrev |> Maybe.map (\prev -> prev.sumNormal)
        }


calculateNormals : Array.Array Vec3 -> Int -> Vec3 -> WithNormals
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
        { index = idx
        , position = v
        , sumNormal = Vec3.add prevNorm nextNorm |> Vec3.normalize
        , prevPosition = prevV
        , prevNormal = prevNorm
        , prevSumNormal = Nothing -- will added later
        }


trianglePairAt : Float -> WithNormals -> ( Triangle, Triangle )
trianglePairAt thickness { prevPosition, prevNormal, prevSumNormal, position, sumNormal, index } =
    let
        v = position
        p1 = prevPosition
        p2 = case prevSumNormal of
            Just prevSumNormal -> Vec3.add p1 prevSumNormal |> Vec3.scale thickness
            Nothing -> Vec3.add p1 prevNormal |> Vec3.scale thickness
        p3 = position
        p4 = Vec3.add p3 sumNormal |> Vec3.scale thickness
    in
        case index % 2 of
            0 -> -- even
                ( ( Vertex p1
                  , Vertex p2
                  , Vertex p3
                  )
                , ( Vertex p3
                  , Vertex p2
                  , Vertex p4
                  )
                )
            _ -> -- odd
                ( ( Vertex p2
                  , Vertex p1
                  , Vertex p4
                  )
                , ( Vertex p4
                  , Vertex p1
                  , Vertex p3
                  )
                )


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
        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * cameraTranslate * cameraRotate  *  vec4(position, 1.0);
            vcolor = vec3(1.0, 1.0, 1.0);
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

