module Blend exposing
    ( Blend(..)
    , produce
    )


import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend as B


type Blend
    = Default
    | Add
    | Subtract
    | Multiply
    | Darken
    | ColourBurn
    | LinearBurn
    | Lighten
    | Screen
    | ColourDodge
    | LinearDodge
    | Overlay
    | SoftLight
    | HardLight
    | VividLight
    | LinearLight
    | PinLight
    | Difference
    | Exclusion
    | Custom
        { r : Float
        , g : Float
        , b : Float
        , color : B.Blender
        , alpha : B.Blender
        }


produce : Blend -> Setting
produce blend =
    B.add B.one B.zero
