module Blend exposing
    ( Blend
    , Color
    , Equation
    , produce
    , default
    , allFuncs
    , allFactors
    , labelOfFunc
    , labelOfFactor
    )


import Array


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


type alias Equation = ( Int, Int, Int ) -- ( 0..2, 0..15, 0..15 )


type alias Color = { r : Float, g : Float, b : Float, a : Float }


type alias Blend
    = { color: Maybe Color
      , colorEq : Equation
      , alphaEq : Equation
      }


default : Blend
default =
    { color = Nothing
    , colorEq = ( 0, 1, 0 ) -- add one zero
    , alphaEq = ( 0, 1, 0 ) -- add one zero
    }


allFuncs : Array.Array (B.Factor -> B.Factor -> B.Blender)
allFuncs =
    Array.fromList
        [ B.customAdd
        , B.customSubtract
        , B.customReverseSubtract
        ]


allFactors : Array.Array B.Factor
allFactors =
    Array.fromList
        [ B.zero
        , B.one
        , B.srcColor
        , B.oneMinusSrcColor
        , B.dstColor
        , B.oneMinusDstColor
        , B.srcAlpha
        , B.oneMinusSrcAlpha
        , B.dstAlpha
        , B.oneMinusDstAlpha
        , B.srcAlphaSaturate
        , B.constantColor
        , B.oneMinusConstantColor
        , B.constantAlpha
        , B.oneMinusConstantAlpha
        ]


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


blenderOf : Equation -> B.Blender
blenderOf ( f, f1, f2 ) =
    let
        f_  = allFuncs |> Array.get f |> Maybe.withDefault B.customAdd
        f1_ = allFactors |> Array.get f1 |> Maybe.withDefault B.one
        f2_ = allFactors |> Array.get f2 |> Maybe.withDefault B.zero
    in
        f_ f1_ f2_


labelOfFunc : Int -> String
labelOfFunc n =
    case n of
        0 -> "+" -- B.customAdd f1_ f2_
        1 -> "-" -- B.customSubtract f1_ f2_
        2 -> "R-" -- B.customReverseSubtract f1_ f2_
        _ -> "?" -- B.customAdd f1_ f2_


labelOfFactor : Int -> String
labelOfFactor n =
    case n of
        0 -> "0" -- B.zero
        1 -> "1" -- B.one
        2 -> "sC" -- B.srcColor
        3 -> "1-sC" -- B.oneMinusSrcColor
        4 -> "dC" -- B.dstColor
        5 -> "1-dC" -- B.oneMinusDstColor
        6 -> "sA" -- B.srcAlpha
        7 -> "1-sA" -- B.oneMinusSrcAlpha
        8 -> "dA" -- B.dstAlpha
        9 -> "1-dA" -- B.oneMinusDstAlpha
        10 -> "AS" -- B.srcAlphaSaturate
        11 -> "CC" -- B.constantColor
        12 -> "1-CC" -- B.oneMinusConstantColor
        13 -> "CA" -- B.constantAlpha
        14 -> "1-CA" -- B.oneMinusConstantAlpha
        _ -> "?"
