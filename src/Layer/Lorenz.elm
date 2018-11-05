module Layer.Lorenz exposing
    ( Model
    , Mesh
    , init
    , makeEntity
    , build
    )

import Array exposing (Array)

import Math.Vector3 as Vec3 exposing (Vec3, vec3, getX, getY, getZ)
import WebGL
import WebGL.Settings exposing (Setting)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Viewport exposing (Viewport)


type alias Triangle = ( Vertex, Vertex, Vertex )


type alias Mesh = WebGL.Mesh Vertex


type alias Model =
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
    , color : Vec3
    }


type alias Uniforms =
    Viewport
        { shade : Float
        }



init : Model
init =
    { sigma = 10
    , beta = 8 / 3
    , rho = 28
    , step = 0.005
    , numVertices = 2000
    , thickness = 1
    }


makeEntity : Viewport {} -> List Setting -> Mesh -> WebGL.Entity
makeEntity viewport settings mesh =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        ( uniforms viewport )


build : Model -> Mesh
build model =
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
        vertices = List.range 1 model.numVertices
            |> List.foldr (\_ positions ->
                case positions of
                    [] -> [ vec3 x0 y0 z0 ]
                    prev :: _ -> (prev |> step model) :: positions
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
            |> Array.map (trianglePairAt model.thickness)
            |> flattenTriangles
            |> WebGL.triangles


flattenTriangles : Array.Array ( Triangle, Triangle ) -> List Triangle
flattenTriangles src =
    src |>
        Array.foldl
            (\( firstInPair, secondInPair ) allTriangles ->
                allTriangles ++ [ firstInPair ] ++ [ secondInPair ]) []



step : Model -> Vec3 -> Vec3
step model v =
    let
        ( x, y, z ) = ( getX v, getY v, getZ v )
        σ = model.sigma
        β = model.beta
        ρ = model.rho
        -- δt = config.dt / 1000
        δt = model.step
        δx = σ * (y - x) * δt
        δy = ( x * (ρ - z) - y ) * δt
        δz = ( x * y - β * z ) * δt
    in
        vec3 (x + δx) (y + δy) (z + δz)


scaleVertex : Vec3 -> Vec3
scaleVertex v = vec3 (getX v / 10) (getY v / 10) (getZ v / 100)


addPrevSumNormals : Array WithNormals -> Int -> WithNormals -> WithNormals
addPrevSumNormals verticesWithNormals idx v =
    let
        maybePrev = verticesWithNormals |> Array.get (idx - 1)
    in
        { v
        | prevSumNormal = maybePrev |> Maybe.map (\prev -> prev.sumNormal)
        }


calculateNormals : Array Vec3 -> Int -> Vec3 -> WithNormals
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
        , prevSumNormal = Nothing -- will be added later
        }


trianglePairAt : Float -> WithNormals -> ( Triangle, Triangle )
trianglePairAt thickness { prevPosition, prevNormal, prevSumNormal, position, sumNormal, index } =
    let
        v = position
        p1 = prevPosition
        p2 = case prevSumNormal of
            Just prevSumNormal -> Vec3.add p1 (prevSumNormal |> Vec3.scale thickness)
            Nothing -> Vec3.add p1 (prevNormal |> Vec3.scale thickness)
        p3 = position
        p4 = Vec3.add p3 (sumNormal |> Vec3.scale thickness)
    in
        case index % 2 of
            0 -> -- even
                ( ( Vertex p1 red
                  , Vertex p2 red
                  , Vertex p3 red
                  )
                , ( Vertex p3 yellow
                  , Vertex p2 yellow
                  , Vertex p4 yellow
                  )
                )
            _ -> -- odd
                ( ( Vertex p2 blue
                  , Vertex p1 blue
                  , Vertex p4 blue
                  )
                , ( Vertex p4 green
                  , Vertex p1 green
                  , Vertex p3 green
                  )
                )


uniforms : Viewport {} -> Uniforms
uniforms v =
    { rotation = v.rotation
    , perspective = v.perspective
    , camera = v.camera
    , shade = 0.8
    , cameraTranslate = v.cameraTranslate
    , cameraRotate = v.cameraRotate
    , size = v.size
    , origin = v.origin
    , paused = v.paused
    }


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
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


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]


red : Vec3
red =
    vec3 1 0 0

green : Vec3
green =
    vec3 0 1 0

blue : Vec3
blue =
    vec3 0 0 1

yellow : Vec3
yellow =
    vec3 1 0 1
