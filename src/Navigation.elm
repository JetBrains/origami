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


applyUrl : Url -> Model -> Model
applyUrl url model =
    let
        _ = Debug.log "applyUrl" url
    in model


prepareUrlFragment : Model -> String
prepareUrlFragment model =
    "#" ++ encodeMode model.mode ++ "/"
        ++ encodeSizeRule (case model.size of (w, h) -> Custom w h)
        |> Debug.log "prepareUrlFragment"


onUrlChange : Url -> Msg
onUrlChange url =
    let
        _ = Debug.log "onUrlChange" url
    in
        case url.fragment of
            Just fragment ->
                if fragment == "" then NoOp else NoOp
            Nothing -> NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest req =
    let
        _ = Debug.log "onUrlRequest" req
    in NoOp
