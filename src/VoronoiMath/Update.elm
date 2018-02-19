module VoronoiMath.Update exposing (..)

import Color exposing (Color)
import Constants
import Delaunay.BowyerWatson
import Geometry.Point
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (..)
import Random.Pcg exposing (..)


type Msg
    = ToggleDistance
    | AddPoint


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
            addPoint (getRandomUniquePoint model) model


addPoint : ( Point, Seed ) -> Model -> Model
addPoint random model =
    let
        point =
            Tuple.first random
    in
    { model
        | points = point :: model.points
        , triangles = Delaunay.BowyerWatson.addPoint point model.triangles
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
            step pointGenerator model.seed
    in
    ( Geometry.Point.roundPoint point, seed )



-- Generators


pointGenerator : Generator Point
pointGenerator =
    map (\point color -> Point point (Just color)) coordinateGenerator |> andMap colorGenerator


coordinateGenerator : Generator Vec2
coordinateGenerator =
    map2 vec2
        (float Constants.coordinateBufferZone
            (Constants.size - Constants.coordinateBufferZone)
        )
        (float Constants.coordinateBufferZone
            (Constants.size - Constants.coordinateBufferZone)
        )


randomColor : Model -> ( Color, Seed )
randomColor model =
    step colorGenerator model.seed


colorGenerator : Generator Color
colorGenerator =
    map3 Color.rgb (int 0 255) (int 0 255) (int 0 255)
