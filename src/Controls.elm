module Controls exposing
    ( Msg(..)
    , controls
    )


import Html.Events exposing (onInput)
import Html exposing (Html, text, div, input, br)
import Html.Attributes as A exposing (width, height, style, type_, min, max, step)

import Layer.Lorenz exposing (Model)


type Msg
    = Configure Model
    | Rotate Float


controls : Int -> Float -> Model -> Html Msg
controls numVertices theta model =
    div [ ]
        [ input [ type_ "range", A.min "10", A.max "10000", A.step "30"
                , onInput (\iStr ->
                    Configure { model
                              | numVertices = String.toInt iStr
                                  |> Maybe.withDefault numVertices
                              }
                    )
                ]
                [ ]
        , text ("vertices : " ++ String.fromInt numVertices)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "1", A.step "0.01"
                , onInput (\fStr ->
                    Rotate (String.toFloat fStr
                            |> Maybe.withDefault theta)) ]
                [ ]
        , text ("theta : " ++ String.fromFloat theta)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "100", A.step "0.1"
                , onInput (\fStr ->
                    Configure { model
                              | sigma = String.toFloat fStr
                                  |> Maybe.withDefault model.sigma
                              }
                    )
                ]
                [ ]
        , text ("sigma : " ++ String.fromFloat model.sigma)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "15", A.step "0.01"
                , onInput (\fStr ->
                    Configure { model
                              | beta = String.toFloat fStr
                                   |> Maybe.withDefault model.beta
                              }
                    )
                ]
                [ ]
        , text ("beta : " ++ String.fromFloat model.beta)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "100", A.step "0.5"
                , onInput (\fStr ->
                    Configure { model
                              | rho = String.toFloat fStr
                                  |> Maybe.withDefault model.rho
                              }
                    )
                ]
                [ ]
        , text ("rho : " ++ String.fromFloat model.rho)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "1", A.step "0.001"
                , onInput (\fStr ->
                    Configure { model
                              | step = String.toFloat fStr
                                  |> Maybe.withDefault model.step
                              }
                    )
                ]
                [ ]
        , text ("step : " ++ String.fromFloat model.step)
        ]
