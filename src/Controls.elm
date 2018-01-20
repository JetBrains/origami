module Controls exposing
    ( Msg(..)
    , controls
    )


import Html.Events exposing (onInput)
import Html exposing (Html, text, div, input, br)
import Html.Attributes as A exposing (width, height, style, type_, min, max, step)


import Lorenz exposing (Config)


type Msg
    = Configure Config
    | Rotate Float


controls : Int -> Float -> Config -> Html Msg
controls numVertices theta config =
    div [ ]
        [ input [ type_ "range", A.min "10", A.max "10000", A.step "30"
                , onInput (\iStr ->
                    Configure { config
                              | numVertices = String.toInt iStr
                                  |> Result.withDefault numVertices
                              }
                    )
                ]
                [ ]
        , text ("vertices : " ++ toString numVertices)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "1", A.step "0.01"
                , onInput (\fStr ->
                    Rotate (String.toFloat fStr
                            |> Result.withDefault theta)) ]
                [ ]
        , text ("theta : " ++ toString theta)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "100", A.step "0.1"
                , onInput (\fStr ->
                    Configure { config
                              | sigma = String.toFloat fStr
                                  |> Result.withDefault config.sigma
                              }
                    )
                ]
                [ ]
        , text ("sigma : " ++ toString config.sigma)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "15", A.step "0.01"
                , onInput (\fStr ->
                    Configure { config
                              | beta = String.toFloat fStr
                                   |> Result.withDefault config.beta
                              }
                    )
                ]
                [ ]
        , text ("beta : " ++ toString config.beta)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "100", A.step "0.5"
                , onInput (\fStr ->
                    Configure { config
                              | rho = String.toFloat fStr
                                  |> Result.withDefault config.rho
                              }
                    )
                ]
                [ ]
        , text ("rho : " ++ toString config.rho)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "1", A.step "0.001"
                , onInput (\fStr ->
                    Configure { config
                              | step = String.toFloat fStr
                                  |> Result.withDefault config.step
                              }
                    )
                ]
                [ ]
        , text ("step : " ++ toString config.step)
        ]
