module Main exposing (main)

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Task exposing (Task)
import Time exposing (Time)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader, Entity)
import Window


numVertices : Int
numVertices = 5000


scale : Float
scale = 1 / 25


type alias LorenzConfig =
    { sigma : Float
    , beta : Float
    , rho : Float
    , stepSize : Float
    , stepsPerFrame : Int
    }


type alias Model =
    { config : LorenzConfig
    , paused : Bool
    , dt : Time
    , lorenz : Mesh Vertex
    }

type alias Vertex =
    { position : Vec3
    , color : Vec3
    }

type Msg
    = Animate Time
    | Resize Window.Size
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        lorenzConfig =
            { sigma = 10
            , beta = 8 / 3
            , rho = 28
            , stepSize = 0.002
            , stepsPerFrame = 3
            }
    in
        (
            { config = lorenzConfig
            , paused = False
            , dt = 0
            , lorenz = lorenz lorenzConfig
            }
        , Cmd.batch
            [ Task.perform Resize Window.size
            ]
        )


lorenz : LorenzConfig -> Mesh Vertex
lorenz config =
    let
        σ = config.sigma
        β = config.beta
        ρ = config.rho
        -- δt = config.dt / 1000
        δt = config.stepSize
        x0 = 0.1
        y0 = 0.1
        z0 = 0.1
        getNext =
            (\(x, y, z) ->
                let
                    δx = σ * (y - x) * δt
                    δy = ( x * (ρ - z) - y ) * δt
                    δz = ( x * y - β * z ) * δt
                in
                    (x + δx, y + δt, z + δz)
            )
        vertices = List.range 1 numVertices
           |> List.foldl (\_ positions ->
                   let
                       len = List.length positions
                       maybePrev = (List.drop (len - 1) positions) |> List.head
                   in
                       case maybePrev of
                           Just prev -> positions ++ [ getNext prev ]
                           Nothing -> [ (x0, y0, z0 ) ]
               ) []
    in
        vertices
            |> List.map
                (\(x, y, z) ->
                    triangleAt x y z
                )
            |> WebGL.triangles


triangleAt : Float -> Float -> Float -> ( Vertex, Vertex, Vertex )
triangleAt x y z =
    let
        tw = 3 / 400 / scale
        th = 3 / 400 / scale
    in
        ( Vertex (vec3 x (y + th / 2) z) (vec3 1 0 0)
        , Vertex (vec3 (x + tw) (y + th / 2) z) (vec3 0 1 0)
        , Vertex (vec3 (x + tw / 2) (y - th / 2) z) (vec3 0 0 1)
        )

view : Model -> Html Msg
view model =
    let
        a = "foo"
    in
        div [ ]
            [ text (toString model.dt ++ a)
            , WebGL.toHtml
                [ width 800
                , height 800
                , style [ ( "display", "block" ) ]
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    model.lorenz
                    { perspective = perspective 1 }
                ]
            ]


perspective : Float -> Mat4
perspective t =
    Mat4.identity
        |> Mat4.scale3 scale scale scale
        |> Mat4.translate3 -10 -10 0
--    Mat4.mul
--        (Mat4.makePerspective 45 1 0.01 100)
--        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model | dt = dt }
            , Cmd.none
            )
        _ -> ( model, Cmd.none )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

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
