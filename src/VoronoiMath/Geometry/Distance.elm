module VoronoiMath.Geometry.Distance exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import VoronoiMath.Model exposing (Distance, Point)


distance : Distance -> Vec2 -> Vec2 -> Float
distance distForm a b =
    case distForm of
        Model.Euclidean ->
            distanceEuclidean a b

        Model.Manhattan ->
            distanceManhattan a b

        Model.Chebyshev ->
            distanceChebyshev a b


distanceEuclidean : Vec2 -> Vec2 -> Float
distanceEuclidean a b =
    sqrt
        (((getX a - getX b) ^ 2)
            + ((getY a - getY b) ^ 2)
        )


distanceManhattan : Vec2 -> Vec2 -> Float
distanceManhattan a b =
    abs (getX a - getX b) + abs (getY a - getY b)


distanceChebyshev : Vec2 -> Vec2 -> Float
distanceChebyshev a b =
    Basics.max (abs (getX a - getX b)) (abs (getY a - getY b))
