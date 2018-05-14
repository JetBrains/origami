module Viewport exposing
    ( State
    , Viewport
    , find
    --, lift
    )


import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (vec3)


type alias State =
    { theta : Float
    -- , t : Float
    , size : ( Int, Int )
    , paused : Bool
    }


type alias Viewport a =
    { a
    | rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    -- , shade : Float
    , cameraTranslate : Mat4
    , cameraRotate : Mat4
    , size : Vec2
    , paused :  Bool
    , mirror : Vec2 -- normal of the mirror
    }


find : State -> Viewport {}
find { theta, size, paused } =
    let
        ( width, height ) = size
    in
        { rotation
            = Mat4.makeRotate (3 * theta) (vec3 0 0 1)
        , perspective
            = Mat4.makePerspective 90 1.5 0.1 3000
        , camera = Mat4.makeLookAt (vec3 0 0 0.5) (vec3 0 0 0) (vec3 1 0 0)
        --, shade = 0.8
        , cameraTranslate = Mat4.makeTranslate (vec3 0 -0.35 0)
        , cameraRotate = Mat4.makeRotate (0.5) (vec3 0 0 1)
        , size = vec2 (toFloat width) (toFloat height)
        , paused = paused
        , mirror = vec2 -1 -1
        }


-- lift : Viewport a -> Viewport a
-- lift v =
--     { rotation = v.rotation
--     , perspective = v.perspective
--     , camera = v.camera
--     --, shade = 0.8
--     , cameraTranslate = v.cameraTranslate
--     , cameraRotate = v.cameraRotate
--     , width = v.width
--     , height = v.height
--     }

-- { perspective = perspective (t / 1000) }
-- perspective : Float -> Mat4
-- perspective t =
--     Mat4.mul
--         (Mat4.makePerspective 45 1 0.01 100)
--         (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))
