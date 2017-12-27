module Models exposing (..)


import WebGL exposing (Mesh)
import Math.Vector3 as Vec3 exposing (Vec3, vec3, getX, getY, getZ)

import Window
import Msgs exposing (..)
import Configs exposing (..)



scale : Float
scale = 1



type alias Model =
    { config : LorenzConfig
    , paused : Bool
    , fps : Int
    , theta : Float
    , lorenz : Mesh Vertex
    , numVertices : Int
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }






lorenz : Int -> LorenzConfig -> Mesh Vertex
lorenz numVertices config =
    let
        x0 = 0.1
        y0 = 0
        z0 = 0
        -- vertices = Debug.log "vertices" (List.range 1 numVertices
        vertices = List.range 1 numVertices
           |> List.foldl (\_ positions ->
                   let
                       len = List.length positions
                       maybePrev = (List.drop (len - 1) positions) |> List.head
                   in
                       case maybePrev of
                           Just prev -> positions ++ [ prev |> step config  ]
                           Nothing -> [ vec3 x0 y0 z0 ]
               ) []
    in
        vertices
            |> List.map triangleAt
            |> WebGL.triangles


step : LorenzConfig -> Vec3 -> Vec3
step config v =
    let
        ( x, y, z ) = ( getX v, getY v, getZ v )
        σ = config.sigma
        β = config.beta
        ρ = config.rho
        -- δt = config.dt / 1000
        δt = config.stepSize
        δx = σ * (y - x) * δt
        δy = ( x * (ρ - z) - y ) * δt
        δz = ( x * y - β * z ) * δt
    in
        vec3 (x + δx) (y + δy) (z + δz)

triangleAt : Vec3 -> ( Vertex, Vertex, Vertex )
triangleAt v =
    let
        x = getX v / 10
        y = getY v / 10
        z = getZ v / 100
        tw = 3 / 400 / scale
        th = 3 / 400 / scale
    in
        ( Vertex (vec3 x (y + th / 2) z) (vec3 1 0 0)
        , Vertex (vec3 (x + tw) (y + th / 2) z) (vec3 0 1 0)
        , Vertex (vec3 (x + tw / 2) (y - th / 2) z) (vec3 0 0 1)
        )
