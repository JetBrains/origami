module View exposing (..)

import Models exposing (..)
import Msgs exposing (..)
import Controls exposing (..)

import Html exposing (Html, text, div, input, br)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader, Entity)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


view : Model -> Html Msg
view ({ config, lorenz } as model) =
    div [ ]
        ( text (toString model.fps ++ "FPS")
          :: controls model
          :: WebGL.toHtml
                [ width 1550
                , height 800
                , style [ ( "display", "block" ) ]
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    model.lorenz
                    ( uniforms 1 model.theta )
                ]

          :: []
        )


uniforms : Float -> Float -> Uniforms
uniforms t theta =
    {
    --    Mat4.identity
    --        |> Mat4.scale3 scale scale scale
    --        |> Mat4.rotate theta (vec3 1 0 0)
    --        --|> Mat4.translate3 -10 -10 0
     rotation =
                Mat4.makeRotate (3 * theta) (vec3 0 1 0)
    , perspective =
                Mat4.makePerspective 95 1.5 0.1 3000

    , camera = Mat4.makeLookAt (vec3 1 0.5 -0.8) (vec3 -0.5 0.1 0) (vec3 0 1 0)
    , shade = 0.8

    }




-- Shaders



type alias Uniforms =
            { rotation : Mat4
            , perspective : Mat4
            , camera : Mat4
            , shade : Float
            }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
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
