module Blend exposing
    ( Blend(..)
    , produce
    )


import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend as B

{- To support complex blending methods like the ones from Photoshop,
   it is required either to have WebGL2 enabled and use MIN/MAX extentions, or
   use special extension in WebGL1, since their formulae are based on testing if source/destination color is minimum/maximum, and not only this:

   https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/blendEquationSeparate
   https://www.andersriggelsen.dk/glblendfunc.php


   It is possible to implement them using shaders though, but in this case we have to have the
   source texture: http://www.nutty.ca/articles/blend_modes/ (here all the conversion formulae
   are listed).
-}


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
    case blend of
        Default -> B.add B.one B.zero
        Add -> B.add B.one B.one
        Subtract -> B.subtract B.one B.one
        Multiply -> B.add B.dstColor B.zero
        _ -> B.add B.one B.zero
