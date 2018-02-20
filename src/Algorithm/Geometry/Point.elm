module Algorithm.Geometry.Point exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Math.Vector3 exposing (Vec3, vec3)



type alias Point =
    { pos : Vec2, color : Maybe Vec3 }


roundPoint : Point -> Point
roundPoint point =
    Point
        (vec2 (Basics.toFloat (round (getX point.pos)))
            (Basics.toFloat (round (getY point.pos)))
        )
        point.color


toString : Point -> String
toString point =
    String.concat
        (List.intersperse
            ","
            [ Basics.toString (getX point.pos)
            , Basics.toString (getY point.pos)
            ]
        )
