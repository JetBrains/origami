module Layer.JbText exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg.Blend as Blend
import Svg.Attributes as SAttrs
import InlineSvg exposing (inline)


{ icon } =
    inline
        { jetbrains = "./jetbrains-text-square.svg"
        }

view : Blend.Blend -> Html a
view blend =
    div
        [ HAttrs.style [("mix-blend-mode", Blend.encode blend)] ]
        [ icon .jetbrains [ SAttrs.class "text-layer text-layer--jetbrains" ]
        ]
