module Algorithm.Delaunay.Triangle exposing (..)


import Algorithm.Geometry.Distance exposing (distanceEuclidean)
import Algorithm.Geometry.Triangle as Triangle exposing (Triangle)
import Algorithm.Geometry.Circle exposing (Circle)
import Algorithm.Geometry.Point exposing (Point)
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
--import Algorithm.Model exposing (Circle, DelaunayTriangle, Edge, Model, Point, Triangle)


type alias DelaunayTriangle =
    { triangle : Triangle, circle : Circle }


-- drawDelaunay : List DelaunayTriangle -> List (Svg msg)
-- drawDelaunay del =
--     [ drawTriangles del ]


-- drawTriangles : List DelaunayTriangle -> Svg msg
-- drawTriangles del =
--     g
--         [ Svg.Attributes.name "triangles" ]
--         (List.map
--             drawTriangle
--             del
--         )


-- drawTriangle : DelaunayTriangle -> Svg msg
-- drawTriangle del =
--     Geometry.Triangle.draw del.triangle


-- drawCircles : List DelaunayTriangle -> Svg msg
-- drawCircles del =
--     g
--         [ Svg.Attributes.name "circles" ]
--         (List.map drawCircle del)


-- drawCircle : DelaunayTriangle -> Svg msg
-- drawCircle del =
--     case del.circle.center of
--         Nothing ->
--             -- TODO: Return a maybe Svg.
--             g [] []

--         Just center ->
--             Svg.circle
--                 [ cx (Basics.toString (getX center))
--                 , cy (Basics.toString (getY center))
--                 , r (Basics.toString del.circle.radius)
--                 , fill "none"
--                 , stroke "grey"
--                 , strokeWidth "0.25"
--                 ]
--                 []


{-| Turns a triangle into a DelaunayTriangle which
contains information about the circumcenter and radius.
-}
getDelaunayTriangle : Triangle -> DelaunayTriangle
getDelaunayTriangle tri =
    let
        circCenter =
            Triangle.findCircumcenter tri
    in
    Circle
        circCenter
        (distanceEuclidean (Maybe.withDefault (vec2 0 0) circCenter) tri.a.pos)
        |> DelaunayTriangle tri


{-| Returns true if the triangles share at least one edge.
-}
isNeighbor : DelaunayTriangle -> DelaunayTriangle -> Bool
isNeighbor a b =
    List.any
        (Triangle.hasEdge a.triangle)
        (Triangle.getEdges b.triangle)


{-| Returns a list of all triangles that are neighbors to the passed triangle.
-}
neighbors : DelaunayTriangle -> List DelaunayTriangle -> List DelaunayTriangle
neighbors triangle triangles =
    let
        -- Don't consider yourself a neighbor.
        trianglesMinusSelf =
            List.filter
                (\x ->
                    Basics.not
                        (Triangle.compareTriangle
                            triangle.triangle
                            x.triangle
                        )
                )
                triangles
    in
    List.filter
        (isNeighbor triangle)
        trianglesMinusSelf


{-| Returns the 'supertriangle' that encompasses all of our valid points
that we will be adding.
-}
defaultTriangles : Float -> List DelaunayTriangle
defaultTriangles size =
    [ getDelaunayTriangle
        (Triangle
            (Point (vec2 0 0) Nothing)
            (Point (vec2 0 size) Nothing)
            (Point (vec2 size size) Nothing)
        )
    , getDelaunayTriangle
        (Triangle
            (Point (vec2 0 0) Nothing)
            (Point (vec2 size 0) Nothing)
            (Point (vec2 size size) Nothing)
        )
    ]


{-| Returns the points comprising the triangle.
-}
getPoints : DelaunayTriangle -> List Point
getPoints triangle =
    [ triangle.triangle.a
    , triangle.triangle.b
    , triangle.triangle.c
    ]


{-| Checks if a DelaunayTriangle's circle contains a point or not.
-}
containsPoint : DelaunayTriangle -> Point -> Bool
containsPoint triangle point =
    case triangle.circle.center of
        Nothing ->
            False

        Just center ->
            distanceEuclidean point.pos center <= triangle.circle.radius
