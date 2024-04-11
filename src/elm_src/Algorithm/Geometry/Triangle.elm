module Algorithm.Geometry.Triangle exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY, vec2)

import Algorithm.Geometry.Edge as Edge exposing (Edge)
import Algorithm.Geometry.Point as Point exposing (Point)
import Algorithm.Geometry.Util as Util


type alias Triangle =
    { a : Point, b : Point, c : Point }


-- draw : Triangle -> Svg msg
-- draw triangle =
--     polyline
--         [ fill "none"
--         , stroke "gray"
--         , strokeWidth "0.1"
--         , points (toString triangle)
--         ]
--         []


{-| Connects a point to an edge, forming a triangle.
-}
retriangulate : Point -> Edge -> Triangle
retriangulate point edge =
    Triangle
        edge.a
        edge.b
        point


{-| Try every combination of points for the circumcenter.
-}
findCircumcenter : Triangle -> Maybe Vec2
findCircumcenter triangle =
    let
        a =
            triangle.a

        b =
            triangle.b

        c =
            triangle.c
    in
    case circumcenter (Triangle a c b) of
        Nothing ->
            case circumcenter (Triangle b a c) of
                Nothing ->
                    circumcenter (Triangle b c a)

                Just center ->
                    Just center

        Just center ->
            Just center


{-| Finds the circumcenter of a triangle (given it's three points).
-}
circumcenter : Triangle -> Maybe Vec2
circumcenter triangle =
    let
        a =
            triangle.a.pos

        b =
            triangle.b.pos

        c =
            triangle.c.pos

        -- AB
        slopeAB : Maybe Float
        slopeAB =
            Util.perpendicularSlope a b

        slopeInterceptAB : Maybe Float
        slopeInterceptAB =
            Util.solveSlopeInterceptForB (Util.midpoint a b) slopeAB

        -- BC
        slopeBC : Maybe Float
        slopeBC =
            Util.perpendicularSlope b c

        slopeInterceptBC : Maybe Float
        slopeInterceptBC =
            Util.solveSlopeInterceptForB (Util.midpoint b c) slopeBC

        -- Solve for x
        x : Maybe Float
        x =
            Maybe.map4 (\siAB siBC slBC slAB -> (siAB - siBC) / (slBC - slAB))
                slopeInterceptAB
                slopeInterceptBC
                slopeBC
                slopeAB
    in
    Maybe.map3 (\x sAB siAB -> vec2 x (sAB * x + siAB))
        x
        slopeAB
        slopeInterceptAB


{-| Returns the three edges comprising the triangle.
-}
getEdges : Triangle -> List Edge
getEdges triangle =
    [ Edge triangle.a triangle.b
    , Edge triangle.b triangle.c
    , Edge triangle.a triangle.c
    ]


{-| Returns the points of a triangle in String form for drawing
the triangle in Svg using a polyline.

returns: [x1,y1 x2,y2 x3,y3 x1,y1]

-}
toString : Triangle -> String
toString tri =
    List.map Point.toString [ tri.a, tri.b, tri.c, tri.a ]
        |> List.intersperse " "
        |> String.concat


{-| Returns true if the triangle contains the edge passed as a parameter.
-}
hasEdge : Triangle -> Edge -> Bool
hasEdge triangle edge =
    List.any (Edge.isEqual edge) (getEdges triangle)


{-| Returns true if the triangles share at least one edge.
-}
isNeighbor : Triangle -> Triangle -> Bool
isNeighbor a b =
    List.any (hasEdge a) (getEdges b)


{-| Returns a list of all triangles that are neighbors to the passed triangle.
-}
neighbors : Triangle -> List Triangle -> List Triangle
neighbors triangle triangles =
    let
        -- Don't consider yourself a neighbor.
        trianglesMinusSelf =
            List.filter (\x -> Basics.not (compareTriangle triangle x)) triangles
    in
    List.filter (isNeighbor triangle) trianglesMinusSelf


{-| Returns true if the triangles have all three edges in common.
-}
compareTriangle : Triangle -> Triangle -> Bool
compareTriangle a b =
    if List.all (hasEdge a) (getEdges b) then
        True
    else
        False
