module Blend exposing
    ( Blend
    , produce
    , default
    )


import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend as B

{- To support complex blending methods like the ones from Photoshop,
   it is required either to have WebGL2 enabled and use MIN/MAX extentions, or
   use special extension in WebGL1, since their formulae are based on testing if source/destination color is minimum/maximum, and not only this:

   https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/blendEquationSeparate
   https://www.andersriggelsen.dk/glblendfunc.php
   https://threejs.org/examples/webgl_materials_blending_custom.html#


   It is possible to implement them using shaders though, but in this case we have to have the
   source texture: http://www.nutty.ca/articles/blend_modes/ (here all the conversion formulae
   are listed).
-}


type alias Equation = ( Int, Int, Int ) -- ( 0..3, 0..15, 0..15 )


type alias Blend
    = { color: Maybe { r : Float, g : Float, b : Float, a : Float }
      , colorEq : Equation
      , alphaEq : Equation
      }


default : Blend
default =
    { color = Nothing
    , colorEq = ( 0, 1, 0 ) -- add one zero
    , alphaEq = ( 0, 1, 0 ) -- add one zero
    }



produce : Blend -> Setting
produce { color, colorEq, alphaEq } =
    let
        c = color |> Maybe.withDefault { r = 0, g = 0, b = 0, a = 0 }
    in
        B.custom
            { r = c.r, g = c.r, b = c.b, a = c.a
            , color = blenderOf colorEq
            , alpha = blenderOf alphaEq
            }


factorOf : Int -> Maybe B.Factor
factorOf n =
    if (n >= 0) && (n <= 15) then
        Just (case n of
            0 -> B.zero
            1 -> B.one
            2 -> B.srcColor
            3 -> B.oneMinusSrcColor
            4 -> B.dstColor
            5 -> B.oneMinusDstColor
            6 -> B.srcAlpha
            7 -> B.oneMinusSrcAlpha
            8 -> B.dstAlpha
            9 -> B.oneMinusDstAlpha
            10 -> B.srcAlphaSaturate
            11 -> B.constantColor
            12 -> B.oneMinusConstantColor
            13 -> B.constantAlpha
            14 -> B.oneMinusConstantAlpha
            _ -> B.zero)
    else
        Nothing



blenderOf : Equation -> B.Blender
blenderOf ( f, f1, f2 ) =
    let
        f1_ = factorOf f1 |> Maybe.withDefault B.one
        f2_ = factorOf f1 |> Maybe.withDefault B.zero
    in
        case f of
            0 -> B.customAdd f1_ f2_
            1 -> B.customSubtract f1_ f2_
            2 -> B.customReverseSubtract f1_ f2_
            _ -> B.customAdd f1_ f2_
