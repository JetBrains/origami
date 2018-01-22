port module BlendsNode exposing (..)

import Dict
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Blend as B


type alias Model =
    { layerCount : Int
    , size: ( Int, Int )
    , blends: Dict.Dict Int B.Blend
    }


type Msg
    = ChangeBlend Int B.Blend
    | ChangeLayerCount Int
    | Resize ( Int, Int )


init : ( Model, Cmd Msg )
init =
    { layerCount =  0
    , size = ( 500, 500 )
    , blends = Dict.empty
    } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBlend layerId newBlend ->
            { model | blends = model.blends |> Dict.insert layerId newBlend } ! []
        ChangeLayerCount newCount -> { model | layerCount = newCount } ! []
        Resize newSize -> { model | size = newSize } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
          )
        , resize Resize
        ]


view : Model -> Html.Html Msg
view { layerCount, size, blends } =
    svg
        (case size of ( w, h ) -> [ width (toString w), height (toString h) ])
        [ rect [ width "200", height "200" ] [] ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port resize : ( ( Int, Int ) -> msg ) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : B.Blend
      }
    -> msg) -> Sub msg
