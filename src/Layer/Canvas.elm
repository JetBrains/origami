module Layer.Canvas exposing
    ( Model
    , init
    , view
    )


import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Viewport exposing (Viewport)


type alias Model = {}


init : Model
init = {}


-- view : Viewport {} -> Html msg
-- view viewport =
view : Html msg
view =
    div [ class "canvas-layer" ] []
