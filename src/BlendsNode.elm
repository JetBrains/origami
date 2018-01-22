port module BlendsNode exposing (..)

import Dict
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Blend as B


type alias Blends = Dict.Dict Int B.Blend


type alias Model =
    { layerCount : Int
    , size: ( Int, Int )
    , blends: Blends
    }


type Msg
    = ChangeBlend Int B.Blend
    | ChangeLayerCount Int
    | Resize ( Int, Int )


renderBlendFrom : Blends -> Int -> Svg Msg
renderBlendFrom blends idx =
    blends
        |> Dict.get idx
        |> Maybe.withDefault B.default
        |> renderBlend idx


renderBlend : Int -> B.Blend -> Svg Msg
renderBlend idx blend =
    g
        [ class "blend" ]
        [ rect [ width "10", height "10", fill (getFill blend) ] []
        , g
            [ class "color-eq" ]
            [ renderEq "color" blend.colorEq ]
        , g
            [ class "alpha-eq" ]
            [ renderEq "alpha" blend.alphaEq ]
        ]


renderEq : String -> B.Equation -> Svg Msg
renderEq eqType ( func, factor1, factor2 ) =
    g
        [ class ("equation equation-" ++ eqType) ]
        [ ]



getFill : B.Blend -> String
getFill { color } =
    color
        |> Maybe.withDefault { r = 0, g = 0, b = 0, a = 0 }
        |> (\c -> "rgba(" ++ toString c.r ++ "," ++ toString c.g ++ ","
                          ++ toString c.b ++ "," ++ toString c.a ++ ")")


init : ( Model, Cmd Msg )
init =
    { layerCount =  0
    , size = ( 100, 100 )
    , blends = Dict.empty
    } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBlend layerId newBlend ->
            { model | blends = model.blends |> Dict.insert layerId newBlend } ! []
        ChangeLayerCount newCount ->
            { model
            | layerCount = newCount
            , blends =
                List.range 0 (model.layerCount - 1)
                    |> List.map (\idx ->
                           ( idx, model.blends |> Dict.get idx |> Maybe.withDefault B.default )
                       )
                    |> Dict.fromList
            } ! []
        Resize newSize -> { model | size = newSize } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
          )
        , resize Resize
        , changeLayerCount ChangeLayerCount
        ]


view : Model -> Html.Html Msg
view { layerCount, size, blends } =
    svg
        (case size of ( w, h ) -> [ width (toString w), height (toString h) ])
        (List.range 0 (layerCount - 1) |> List.map (renderBlendFrom blends) )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port changeLayerCount : (Int -> msg) -> Sub msg

port resize : ( ( Int, Int ) -> msg ) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : B.Blend
      }
    -> msg) -> Sub msg
