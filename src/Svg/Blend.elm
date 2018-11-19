module Svg.Blend exposing
    ( Blend(..)
    , default
    , encode
    , decode
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


-- TODO: use Map Blend PortBlend
encode : Blend -> String
encode blend =
    case blend of
        Normal -> "normal"
        Difference -> "difference"
        Exclusion -> "exclusion"
        Overlay -> "overlay"
        SoftLight -> "soft-light"
        Hue -> "hue"
        Multiply -> "multiply"
        Screen -> "screen"
        Darken -> "darken"
        Lighten -> "lighten"
        ColorDodge -> "color-dodge"
        ColorBurn -> "color-burn"
        HardLight -> "hard-light"
        Saturation -> "saturation"
        Color -> "color"
        Luminosity -> "luminosity"
        Unset -> "unset"


-- TODO: use Map PortBlend Blend
decode : String -> Blend
decode portBlend =
    case portBlend of
        "normal" -> Normal
        "difference" -> Difference
        "exclusion" -> Exclusion
        "overlay" -> Overlay
        "soft-light" -> SoftLight
        "hue" -> Hue
        "multiply" -> Multiply
        "screen" -> Screen
        "darken" -> Darken
        "lighten" -> Lighten
        "color-dodge" -> ColorDodge
        "color-burn" -> ColorBurn
        "hard-light" -> HardLight
        "saturation" -> Saturation
        "color" -> Color
        "luminosity" -> Luminosity
        "unset" -> Unset
        _ -> Unset


default : Blend
default = Normal
