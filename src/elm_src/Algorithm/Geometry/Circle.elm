module Algorithm.Geometry.Circle exposing (..)

import Math.Vector2 exposing (Vec2)


type alias Circle =
    { center : Maybe Vec2, radius : Float }
