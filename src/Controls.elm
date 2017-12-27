module Controls exposing (..)
import Models exposing (..)
import Msgs exposing (..)
import Html.Events exposing (onInput)
import Html exposing (Html, text, div, input, br)
import Html.Attributes as A exposing (width, height, style, type_, min, max, step)

controls : Model -> Html Msg
controls ({ config, lorenz } as model) =
    div [ ]
        [ input [ type_ "range", A.min "10", A.max "10000", A.step "30"
                , onInput (\iStr ->
                    AdjustVertices (String.toInt iStr
                                    |> Result.withDefault model.numVertices)) ]
                [ ]
        , text ("vertices : " ++ toString model.numVertices)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "1", A.step "0.01"
                , onInput (\fStr ->
                    Rotate (String.toFloat fStr
                            |> Result.withDefault model.theta)) ]
                [ ]
        , text ("theta : " ++ toString model.theta)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "100", A.step "0.1"
                , onInput (\fStr ->
                    ChangeConfig { config
                                    | sigma = String.toFloat fStr
                                            |> Result.withDefault config.sigma
                                    }
                    )
                ]
                [ ]
        , text ("sigma : " ++ toString model.config.sigma)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "15", A.step "0.01"
                , onInput (\fStr ->
                    ChangeConfig { config
                                    | beta = String.toFloat fStr
                                            |> Result.withDefault config.beta
                                    }
                    )
                ]
                [ ]
        , text ("beta : " ++ toString model.config.beta)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "100", A.step "0.5"
                , onInput (\fStr ->
                    ChangeConfig { config
                                    | rho = String.toFloat fStr
                                            |> Result.withDefault config.rho
                                    }
                    )
                ]
                [ ]
        , text ("rho : " ++ toString model.config.rho)
        , br [] []
        , input [ type_ "range", A.min "0", A.max "1", A.step "0.001"
                , onInput (\fStr ->
                    ChangeConfig { config
                                    | stepSize = String.toFloat fStr
                                            |> Result.withDefault config.stepSize
                                    }
                    )
                ]
                [ ]
        , text ("step : " ++ toString model.config.stepSize)
        ]