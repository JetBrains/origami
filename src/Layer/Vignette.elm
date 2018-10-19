module Layer.Vignette exposing
    ( Config
    , Mesh
    , makeEntity
    , init
    )

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL
import WebGL.Settings exposing (Setting)

import Viewport exposing (Viewport)


type alias Config =
    { opacity : Float
    , color : Color
    }


type alias Mesh = WebGL.Mesh Vertex
type alias Color = ( Float, Float, Float )



init : Config
init =
    { opacity = 1.0
    , color = ( 0.0, 0.0, 0.0 )
    }


makeEntity : Viewport {} -> Config -> List Setting -> WebGL.Entity
makeEntity viewport config settings =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms viewport config)



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    Viewport
        { uColor : Vec3
        , uOpacity : Float
        , uResolution : Vec3
        }


uniforms : Viewport {} -> Config -> Uniforms
uniforms v { opacity, color } =
    let
        ( r, g, b ) = color
    in
    -- { perspective = Mat4.mul v.perspective v.camera }
       { rotation = v.rotation
       , perspective = v.perspective
       , camera = v.camera
       , cameraTranslate = v.cameraTranslate
       , cameraRotate = v.cameraRotate
       , size = v.size
       , origin = v.origin
       , paused = v.paused
       , uOpacity = opacity
       , uColor = vec3 r g b
       , uResolution = vec3 2000.0 2000.0 0
       }


vertexShader : WebGL.Shader Vertex Uniforms { }
vertexShader =
    [glsl|

        attribute vec3 position;

        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;

        //varying vec3 vColor;

        void main () {
            // gl_Position = perspective * camera * rotation * cameraTranslate * cameraRotate * vec4(position, 1.0);
            // gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            //vColor = color;
        }

    |]


fragmentShader : WebGL.Shader {} Uniforms {}
fragmentShader =
    [glsl|

        // Precision
        precision mediump float;

        uniform vec3 uColor;
        uniform float uOpacity;
        uniform vec3 uResolution;

        vec3 vignette() {

            vec2 st = gl_FragCoord.xy/uResolution.xy;
             float pct = 0.0;

             // a. The DISTANCE from the pixel to the center
             pct = distance(st,vec2(0.5));

             // b. The LENGTH of the vector
             //    from the pixel to the center
             // vec2 toCenter = vec2(0.5)-st;
             // pct = length(toCenter);

             // c. The SQUARE ROOT of the vector
             //    from the pixel to the center
             // vec2 tC = vec2(0.5)-st;
             // pct = sqrt(tC.x*tC.x+tC.y*tC.y);

            return vec3(pct, 0, 0);

        }

        // Main
        void main() {


            // Set gl_FragColor
            gl_FragColor = vec4(uColor, 1.0);


            gl_FragColor += vec4( vignette(), 1.0 );

        }
    |]
