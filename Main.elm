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
numVertices = 500


type alias Model =
    { sigma : Float
    , beta : Float
    , rho : Float
    , stepSize : Float
    , stepsPerFrame : Int
    , paused : Bool
    , dt : Time
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
    (
        { sigma = 10
        , beta = 8 / 3
        , rho = 28
        , stepSize = 0.002
        , stepsPerFrame = 3
        , paused = False
        , dt = 0
        }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )


lorenz : Model -> Mesh Vertex
lorenz model =
    let
        σ = model.sigma
        β = model.beta
        ρ = model.rho
        δt = model.dt

    in
        List.range 1 numVertices
            |> List.map
                (\i -> triangleAt (1 - (toFloat i / toFloat numVertices) * 2) 0)
            |> WebGL.triangles


triangleAt : Float -> Float -> ( Vertex, Vertex, Vertex )
triangleAt x y =
    let
        tw = 3 / 400
        th = 3 / 400
    in
        ( Vertex (vec3 x (y + th / 2) 0) (vec3 1 0 0)
        , Vertex (vec3 (x + tw) (y + th / 2) 0) (vec3 0 1 0)
        , Vertex (vec3 (x + tw / 2) (y - th / 2) 0) (vec3 0 0 1)
        )

view : Model -> Html Msg
view model =
    let
        a = "foo"
    in
        div [ ]
            [ text (toString model.sigma ++ a)
            , WebGL.toHtml
                [ width 800
                , height 800
                , style [ ( "display", "block" ) ]
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    (lorenz model)
                    { perspective = perspective 1 }
                ]
            ]


perspective : Float -> Mat4
perspective t =
    Mat4.identity
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
            ( { model | sigma = dt, dt = dt }
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
