module Algorithm.Voronoi.Model exposing (..)

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random.Pcg exposing (..)

import Algorithm.Geometry.Point exposing (Point)
import Algorithm.Geometry.Triangle exposing (Triangle, findCircumcenter)
import Algorithm.Geometry.Distance as Distance
import Algorithm.Geometry.Edge exposing (Edge)
import Algorithm.Geometry.Circle exposing (Circle)


type alias VoronoiPolygon =
    { edges : List Edge, color : Maybe Vec3 }


type alias DelaunayTriangle =
    { triangle : Triangle, circle : Circle }



type alias Model =
    { distance : Distance
    , points : List Point
    , triangles : List DelaunayTriangle
    , seed : Seed
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
    }


{-| Turns a triangle into a DelaunayTriangle which
contains information about the circumcenter and radius.
-}
getDelaunayTriangle : Triangle -> Model.DelaunayTriangle
getDelaunayTriangle tri =
    let
        circCenter =
            findCircumcenter tri
    in
    Model.Circle
        circCenter
        (Distance.distanceEuclidean (Maybe.withDefault (vec2 0 0) circCenter) tri.a.pos)
        |> Model.DelaunayTriangle tri


distance : Distance -> Vec2 -> Vec2 -> Float
distance distForm a b =
    case distForm of
        Euclidean ->
            Distance.distanceEuclidean a b

        Manhattan ->
            Distance.distanceManhattan a b

        Chebyshev ->
            Distance.distanceChebyshev a b
