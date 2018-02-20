module Algorithm.Voronoi.Update exposing (..)

import Color exposing (Color)
import Algorithm.Delaunay.BowyerWatson as BowyerWatson
import Algorithm.Geometry.Point exposing (Point, roundPoint)
import Algorithm.Voronoi.Model exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Random.Pcg exposing (..)


type Msg
    = ToggleDistance
    | AddPoint
    | ChangeSize Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDistance ->
            case model.distance of
                Euclidean ->
                    { model | distance = Manhattan }

                Manhattan ->
                    { model | distance = Chebyshev }

                Chebyshev ->
                    { model | distance = Euclidean }

        AddPoint ->
            addPoint model.size (getRandomUniquePoint model) model

        ChangeSize size ->
            { model | size = size }


addPoint : Float -> ( Point, Seed ) -> Model -> Model
addPoint size random model =
    let
        point =
            Tuple.first random
    in
    { model
        | points = point :: model.points
        , triangles = BowyerWatson.addPoint size point model.triangles
    }


updateSeed : ( Point, Seed ) -> Model -> Model
updateSeed random model =
    { model | seed = Tuple.second random }


{-| Get a random point that is not being occupied by another point.
-}
getRandomUniquePoint : Model -> ( Point, Seed )
getRandomUniquePoint model =
    let
        point =
            randomPoint model
    in
    if pointIsUnique model.points (Tuple.first point) then
        -- Make sure to change the seed
        -- so we don't keep trying the same point.
        getRandomUniquePoint (model |> updateSeed point)
    else
        point


{-| Checks if a list of points contains a point's current location.
-}
pointIsUnique : List Point -> Point -> Bool
pointIsUnique points newPoint =
    List.any (\point -> point.pos == newPoint.pos) points


randomPoint : Model -> ( Point, Seed )
randomPoint model =
    let
        ( point, seed ) =
            step (pointGenerator model.size) model.seed
    in
    ( roundPoint point, seed )



-- Generators


pointGenerator : Float -> Generator Point
pointGenerator size =
    map (\point color -> Point point (Just color))
        (coordinateGenerator size 10) |> andMap colorGenerator


coordinateGenerator : Float -> Float -> Generator Vec2
coordinateGenerator size bufferZone =
    map2 vec2
        (float bufferZone
            (size - bufferZone)
        )
        (float bufferZone
            (size - bufferZone)
        )


randomColor : Model -> ( Color, Seed )
randomColor model =
    step colorGenerator model.seed


colorGenerator : Generator Color
colorGenerator =
    map3 Color.rgb (int 0 255) (int 0 255) (int 0 255)
