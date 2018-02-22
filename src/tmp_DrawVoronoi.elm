module DrawVoronoi exposing (..)


a : Int -> Int -> Int
a b c = b + c

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


-- averageColor : Triangle -> String
-- averageColor tri =
--     let
--         a =
--             Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) tri.a.color)

--         b =
--             Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) tri.b.color)

--         c =
--             Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) tri.c.color)
--     in
--     ColorHelper.colorToHex
--         (Color.rgb
--             (round (sqrt (Basics.toFloat ((a.red + b.red + c.red) ^ 2) / 3)))
--             (round (sqrt (Basics.toFloat ((a.green + b.green + c.green) ^ 2) / 3)))
--             (round (sqrt (Basics.toFloat ((a.blue + b.blue + c.blue) ^ 2) / 3)))
--         )


-- drawPoints : Model -> List (Svg msg)
-- drawPoints model =
--     List.map draw model.points


-- draw : Point -> Svg msg
-- draw point =
--     -- TODO: Don't draw maybe point
--     let
--         color =
--             Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) point.color)
--     in
--     circle
--         [ cx <| Basics.toString <| getX point.pos
--         , cy <| Basics.toString <| getY point.pos
--         , r "4"
--         , fill <|
--             colorToHex
--                 (Color.rgb
--                     (round (Constants.pointColorMult * Basics.toFloat color.red))
--                     (round (Constants.pointColorMult * Basics.toFloat color.green))
--                     (round (Constants.pointColorMult * Basics.toFloat color.blue))
--                 )
--         , stroke "black"
--         , strokeWidth "0.25"
--         ]
--         []


-- averageColor : Edge -> String
-- averageColor edge =
--     let
--         a =
--             Color.toRgb (Maybe.withDefault (Color.rgb 255 0 0) edge.a.color)

--         b =
--             Color.toRgb (Maybe.withDefault (Color.rgb 0 0 255) edge.b.color)
--     in
--     ColorHelper.colorToHex
--         (Color.rgb
--             (round (sqrt (Basics.toFloat ((a.red + b.red) ^ 2) / 2)))
--             (round (sqrt (Basics.toFloat ((a.green + b.green) ^ 2) / 2)))
--             (round (sqrt (Basics.toFloat ((a.blue + b.blue) ^ 2) / 2)))
--         )


-- toString : Edge -> String
-- toString edge =
--     Basics.toString (getX edge.a.pos)
--         ++ ","
--         ++ Basics.toString (getY edge.a.pos)
--         ++ " "
--         ++ Basics.toString (getX edge.b.pos)
--         ++ ","
--         ++ Basics.toString (getY edge.b.pos)
