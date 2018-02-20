module Algorithm.Voronoi.Model exposing (..)

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random.Pcg exposing (..)

import Algorithm.Geometry.Point exposing (Point)
import Algorithm.Delaunay.Triangle exposing (DelaunayTriangle)
import Algorithm.Geometry.Distance as Distance
import Algorithm.Geometry.Edge exposing (Edge)


type alias VoronoiPolygon =
    { edges : List Edge, color : Maybe Vec3 }


type alias Model =
    { distance : Distance
    , points : List Point
    , triangles : List DelaunayTriangle
    , seed : Seed
    , size : Float
    }


type Distance
    = Euclidean
    | Manhattan
    | Chebyshev


init : Model
init =
    { distance = Euclidean
    , points = []
    , triangles = []
    , seed = initialSeed 3178909195
    , size = 500
    }


distance : Distance -> Vec2 -> Vec2 -> Float
distance distForm a b =
    case distForm of
        Euclidean ->
            Distance.distanceEuclidean a b

        Manhattan ->
            Distance.distanceManhattan a b

        Chebyshev ->
            Distance.distanceChebyshev a b
