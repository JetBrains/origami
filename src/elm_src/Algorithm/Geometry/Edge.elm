module Algorithm.Geometry.Edge exposing (..)


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
