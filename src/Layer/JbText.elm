module Layer.JbText exposing
    ( view
    )

import Html exposing (..)
import Svg.Attributes
import InlineSvg exposing (inline)


{ icon } =
    inline
        { jetbrains = "jetbrains-text-square.svg"
        }

view =
    div
        []
        [ icon .jetbrains [ Svg.Attributes.class "text text--jetbrains" ]
        ]
