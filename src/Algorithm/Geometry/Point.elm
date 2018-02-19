module Algorithm.Geometry.Point exposing (..)

import Math.Vector2 exposing (getX, getY, vec2)
import Algorithm.Voronoi.Model exposing (Model, Point)


-- View


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



-- Controller


roundPoint : Point -> Point
roundPoint point =
    Point
        (vec2 (Basics.toFloat (round (getX point.pos)))
            (Basics.toFloat (round (getY point.pos)))
        )
        point.color


toString : Point -> String
toString point =
    String.concat
        (List.intersperse
            ","
            [ Basics.toString (getX point.pos)
            , Basics.toString (getY point.pos)
            ]
        )
