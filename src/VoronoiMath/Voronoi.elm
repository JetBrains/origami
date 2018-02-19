module VoronoiMath.Voronoi exposing (..)

import VoronoiMath.Delaunay.Triangle
import Geometry.Edge
import Model exposing (DelaunayTriangle, Edge, Model, Point, VoronoiPolygon)


-- draw : Model -> List (Svg msg)
-- draw model =
--     let
--         voronoi =
--             get model
--     in
--     --List.map Geometry.Edge.draw edges
--     List.map drawVoronoi voronoi


-- drawVoronoi : VoronoiPolygon -> Svg msg
-- drawVoronoi voronoi =
--     polyline
--         [ fill "gray"
--         , stroke "black"
--         , points (toString voronoi)
--         ]
--         []


-- {-| Returns the points of a VoronoiPolygon in String form for drawing
-- the triangle in Svg using a polyline.
-- -}
-- toString : VoronoiPolygon -> String
-- toString voronoi =
--     String.concat (List.intersperse " " (List.map (\edge -> Geometry.Edge.toString edge) voronoi.edges))


{-| For each triangle in our triangulation, connect the triangle circumcenter
to it's neighboring triangle's circumcenter.
-}
get : Model -> List VoronoiPolygon
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
    let
        neighbors =
            Delaunay.Triangle.neighbors triangle triangles
    in
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
    if Delaunay.Triangle.isNeighbor a b then
        Maybe.map2
            (\centerA centerB -> Edge (Point centerA Nothing) (Point centerB Nothing))
            a.circle.center
            b.circle.center
    else
        Nothing
