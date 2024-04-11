module Algorithm.Geometry.Util exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY, vec2)


{-| Finds the slope between two points.
-}
slope : Vec2 -> Vec2 -> Maybe Float
slope from to =
    if getX to == getX from then
        -- The slope of a line perpendicular to that with undefined slope is 0
        Nothing
    else
        Just ((getY to - getY from) / (getX to - getX from))


{-| Finds the slope of the perpendicular bisector
for two points.
-}
perpendicularSlope : Vec2 -> Vec2 -> Maybe Float
perpendicularSlope from to =
    case slope from to of
        Nothing ->
            -- The slope of a line perpendicular to that with undefined slope is 0
            Just 0

        Just slopeResult ->
            if slopeResult == 0 then
                Nothing
            else
                Just (-1 / slopeResult)


{-| Finds the midpoint between two points.
-}
midpoint : Vec2 -> Vec2 -> Vec2
midpoint a b =
    vec2 ((getX a + getX b) / 2) ((getY a + getY b) / 2)


{-| Solves y=mx+b for b
-}
solveSlopeInterceptForB : Vec2 -> Maybe Float -> Maybe Float
solveSlopeInterceptForB point slope =
    case slope of
        Nothing ->
            -- A vertical line won't intercept y
            Nothing

        Just slope ->
            Just (getY point - slope * getX point)
