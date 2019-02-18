module Navigation exposing
    ( applyUrl
    , prepareUrlFragment
    , onUrlChange
    , onUrlRequest
    )

import Browser
import Browser.Navigation exposing (..)
import Url exposing (..)

import Model exposing (..)


type alias Fragment = String


applyFragment : Fragment -> Model -> Model
applyFragment _ model = model


fragmentToMessage : Fragment -> Msg
fragmentToMessage _ = NoOp


applyUrl : Url -> Model -> Model
applyUrl url model =
    let
        _ = Debug.log "applyUrl" url
    in case url.fragment of
        Just fragment -> model |> applyFragment fragment
        Nothing ->
            { model | size = Dimensionless }


prepareUrlFragment : Model -> Fragment
prepareUrlFragment model =
    encodeMode model.mode ++ "/"
    ++ encodeSizeRule model.size


onUrlChange : Url -> Msg
onUrlChange url =
    let
        _ = Debug.log "onUrlChange" url
    in
        case url.fragment of
            Just fragment -> fragmentToMessage fragment
            Nothing -> NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest req =
    let
        _ = Debug.log "onUrlRequest" req
    in NoOp
