module Svg.Blend exposing
    ( Blend
    , default
    )

type Blend
    = Normal
    | Difference
    | Exclusion
    | Overlay
    | SoftLight
    | Hue
    | Multiply
    | Screen
    | Darken
    | Lighten
    | ColorDodge
    | ColorBurn
    | HardLight
    | Saturation
    | Color
    | Luminosity
    | Unset


default : Blend
default = Normal
