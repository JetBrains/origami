module Canvas.Blend exposing
    ( Blend )

-- /* globalCompositeOperation :
--   normal | multiply | screen | overlay |
--   darken | lighten | color-dodge | color-burn | hard-light |
--   soft-light | difference | exclusion | hue | saturation |
--   color | luminosity

-- mix-blend-mode: normal;
-- mix-blend-mode: multiply;
-- mix-blend-mode: screen;
-- mix-blend-mode: overlay;
-- mix-blend-mode: darken;
-- mix-blend-mode: lighten;
-- mix-blend-mode: color-dodge;
-- mix-blend-mode: color-burn;
-- mix-blend-mode: hard-light;
-- mix-blend-mode: soft-light;
-- mix-blend-mode: difference;
-- mix-blend-mode: exclusion;
-- mix-blend-mode: hue;
-- mix-blend-mode: saturation;
-- mix-blend-mode: color;
-- mix-blend-mode: luminosity;

-- /* Global values */
-- mix-blend-mode: initial;
-- mix-blend-mode: inherit;
-- mix-blend-mode: unset;

type alias Blend
    = Normal
    | Multiply
    | Screen
    | Overlay
    | Darken
    | Lighten
    | ColorDodge
    | ColorBurn
    | HardLight
    | SoftLight
    | Difference
    | Exclusion
    | Hue
    | Saturation
    | Color
    | Luminosity


