module Algorithm.Voronoi exposing (..)


import Algorithm.Geometry.Point exposing (Point)
import Algorithm.Delaunay.Triangle exposing (DelaunayTriangle, neighbors, isNeighbor)
import Algorithm.Geometry.Distance as Distance
import Algorithm.Geometry.Edge exposing (Edge)

import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random.Pcg exposing (Seed, Generator, initialSeed, step, map, map2, map3, andMap, int, float)

import Algorithm.Delaunay.BowyerWatson as BowyerWatson
import Algorithm.Geometry.Point exposing (Point, roundPoint)


type alias Color = Vec3


type Msg
    = SetDistance Distance
    | AddRandomPoint
    | AddPoint Point
    | ChangeSize Float


type alias VoronoiPolygon =
    { edges : List Edge, color : Maybe Vec3 }


type alias Voronoi =
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


build : Float -> Int -> Distance -> Voronoi
build size pointCount distance =
    let
        initial = init |> update (ChangeSize size)
    in
    -- FIXME: just pass random point with AddPoint and so build them before
    --        (or it has no sense?)
    List.range 0 pointCount |>
        List.foldr (\_ model -> model |> update AddRandomPoint) initial


init : Voronoi
init =
    { distance = Euclidean
    , points = []
    , triangles = []
    , seed = initialSeed 3178909195
    , size = 500
    }


update : Msg -> Voronoi -> Voronoi
update msg model =
    case msg of
        SetDistance distance ->
            { model | distance = distance }

        AddPoint point ->
            model |> addPoint point

        AddRandomPoint ->
            let
                random = getRandomUniquePoint model
            in
            { model | seed = Tuple.second random }
                |> addPoint (Tuple.first random)

        ChangeSize size ->
            { model | size = size }


distance : Distance -> Vec2 -> Vec2 -> Float
distance distForm a b =
    case distForm of
        Euclidean ->
            Distance.distanceEuclidean a b

        Manhattan ->
            Distance.distanceManhattan a b

        Chebyshev ->
            Distance.distanceChebyshev a b


{-| For each triangle in our triangulation, connect the triangle circumcenter
to it's neighboring triangle's circumcenter.
-}
get : Voronoi -> List VoronoiPolygon
get model =
    getVoronoi model.triangles


{-| Calls the getVoronoiRecurse function on the first triangle in the list.
This methods excludes the triangle from all future calls.
-}
getVoronoi : List DelaunayTriangle -> List VoronoiPolygon
getVoronoi triangles =
    Maybe.withDefault []
        (Maybe.map2
            (getVoronoiRecurse [])
            (List.head triangles)
            (List.tail triangles)
        )


{-| Recursively calls itself until there are no more triangles left.
This method will avoid re-creating edges that are already created by
not including the triangle operated on in the future calls, making it
so that it's no longer the neighbor for any other triangles.
-}
getVoronoiRecurse : List VoronoiPolygon -> DelaunayTriangle -> List DelaunayTriangle -> List VoronoiPolygon
getVoronoiRecurse voronoi triangle triangles =
    -- let
    --     delaunayNeighbors =
    --         neighbors triangle triangles
    -- in
    List.append
        [ VoronoiPolygon (List.filterMap (connectTriangles triangle) triangles) Nothing ]
        (Maybe.withDefault voronoi
            (Maybe.map2
                (getVoronoiRecurse voronoi)
                (List.head triangles)
                (List.tail triangles)
            )
        )


{-| Connects two DelaunayTriangles together by their circumcenters.
Returns the edge between the cirumcenters.
-}
connectTriangles : DelaunayTriangle -> DelaunayTriangle -> Maybe Edge
connectTriangles a b =
    if isNeighbor a b then
        Maybe.map2
            (\centerA centerB -> Edge (Point centerA Nothing) (Point centerB Nothing))
            a.circle.center
            b.circle.center
    else
        Nothing


addPoint : Point -> Voronoi -> Voronoi
addPoint point model =
    { model
        | points = point :: model.points
        , triangles = BowyerWatson.addPoint model.size point model.triangles
    }


updateSeed : Seed -> Voronoi -> Voronoi
updateSeed seed model =
    { model | seed = seed }


{-| Get a random point that is not being occupied by another point.
-}
getRandomUniquePoint : Voronoi -> ( Point, Seed )
getRandomUniquePoint model =
    let
        random =
            randomPoint model.size model.seed
    in
    if pointIsUnique model.points (Tuple.first random) then
        -- Make sure to change the seed
        -- so we don't keep trying the same point.
        getRandomUniquePoint (model |> updateSeed (Tuple.second random))
    else
        random


{-| Checks if a list of points contains a point's current location.
-}
pointIsUnique : List Point -> Point -> Bool
pointIsUnique points newPoint =
    List.any (\point -> point.pos == newPoint.pos) points


randomPoint : Float -> Seed -> ( Point, Seed )
randomPoint size seed =
    let
        ( point, newSeed ) =
            step (pointGenerator size) seed
    in
    ( roundPoint point, newSeed )



-- Generators


pointGenerator : Float -> Generator Point
pointGenerator size =
    let
        coordGen = coordinateGenerator size 10
    in
        map (\point color -> Point point (Just color))
            coordGen |> andMap colorGenerator


coordinateGenerator : Float -> Float -> Generator Vec2
coordinateGenerator size bufferZone =
    map2 vec2
        (float bufferZone
            (size - bufferZone)
        )
        (float bufferZone
            (size - bufferZone)
        )


randomColor : Voronoi -> ( Color, Seed )
randomColor model =
    step colorGenerator model.seed


colorGenerator : Generator Color
colorGenerator =
    map3 vec3 (float 0 1) (float 0 1) (float 0 1)
