module Algorithm.Geometry.Edge exposing (..)

import Math.Vector2 exposing (getX, getY)
import Algorithm.Geometry.Point exposing (Point)


type alias Edge =
    { a : Point, b : Point }


-- draw : Edge -> Svg msg
-- draw edge =
--     line
--         [ stroke "black"
--         , strokeWidth "0.5"
--         , x1 (Basics.toString (getX edge.a.pos))
--         , x2 (Basics.toString (getX edge.b.pos))
--         , y1 (Basics.toString (getY edge.a.pos))
--         , y2 (Basics.toString (getY edge.b.pos))
--         ]
--         []


getUnique : List Edge -> List Edge
getUnique edges =
    let
        duplicates =
            getDuplicates edges
    in
    List.filter (\x -> Basics.not (contains duplicates x)) edges


getDuplicates : List Edge -> List Edge
getDuplicates edges =
    case List.tail edges of
        Nothing ->
            []

        Just tail ->
            case List.head edges of
                Nothing ->
                    []

                Just head ->
                    List.append
                        (getDuplicate tail head)
                        (getDuplicates (List.drop 1 edges))


getDuplicate : List Edge -> Edge -> List Edge
getDuplicate edges edge =
    if contains edges edge then
        [ edge ]
    else
        []


contains : List Edge -> Edge -> Bool
contains edges edge =
    List.any (\x -> isEqual edge x) edges


isEqual : Edge -> Edge -> Bool
isEqual a b =
    if (a.a == b.a) && (a.b == b.b) then
        True
    else if (a.a == b.b) && (a.b == b.a) then
        True
    else
        False


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
