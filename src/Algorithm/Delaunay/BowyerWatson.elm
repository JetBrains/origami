module Algorithm.Delaunay.BowyerWatson exposing (addPoint)

import Algorithm.Delaunay.Triangle as DelaunayTriangle exposing
    ( DelaunayTriangle
    , getDelaunayTriangle
    , defaultTriangles
    , containsPoint
    )
import Algorithm.Geometry.Edge as Edge exposing (Edge)
import Algorithm.Geometry.Point exposing (Point)
import Algorithm.Geometry.Triangle as Triangle exposing (Triangle, retriangulate)


{-| (Assuming a basic understanding of the Bowyer Watson algorithm)
We start off with our default triangles if our triangle list passed is
empty. Otherwise we add it to the existing list.

When we add the point, we are going to remove the bad triangles and
re-triangulate the polygonal hole created by the freshly added point.
We do that by connecting the point to the unique edges of the bad
triangles.

-}
addPoint : Float -> Point -> List DelaunayTriangle -> List DelaunayTriangle
addPoint size point triangles =
    if triangles == [] then
        goodTriangles point (defaultTriangles size)
            |> retriangulatePolygonalHole point (badTriangleEdges point (defaultTriangles size))
    else
        goodTriangles point triangles
            |> retriangulatePolygonalHole point (badTriangleEdges point triangles)


{-| Connect the pont to every edge, and add it into the list of triangles.
-}
retriangulatePolygonalHole : Point -> List Edge -> List DelaunayTriangle -> List DelaunayTriangle
retriangulatePolygonalHole point edges triangles =
    List.append
        triangles
        (List.map
            (\edge ->
                getDelaunayTriangle (retriangulate point edge)
            )
            edges
        )



-- Utils


{-| Returns ONLY unique edges between all of the bad triangles
found in the triangle list.

For example, if two triangles share an edge, then the edge is NOT included.
However, if an edge is included only once, then it is included.

-}
badTriangleEdges : Point -> List DelaunayTriangle -> List Edge
badTriangleEdges point triangles =
    List.map
        (\tri -> Triangle.getEdges tri.triangle)
        (badTriangles point triangles)
        |> List.concat
        |> Edge.getUnique


{-| Returns triangles that contain the point.
-}
badTriangles : Point -> List DelaunayTriangle -> List DelaunayTriangle
badTriangles point triangulation =
    let
        isBad point triangle =
            if containsPoint triangle point then
                Just triangle
            else
                Nothing
    in
    List.filterMap (isBad point) triangulation


{-| Returns triangles that do not contain the point.
-}
goodTriangles : Point -> List DelaunayTriangle -> List DelaunayTriangle
goodTriangles point triangulation =
    let
        isGood point triangle =
            if containsPoint triangle point then
                Nothing
            else
                Just triangle
    in
    List.filterMap (isGood point) triangulation
