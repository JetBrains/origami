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
-- type ModeFragment = ModeFragment String
-- type SizeRuleFragment = SizeRuleFragment
--     { ruleStr: String
--     , size: Maybe ( Int, Int )
--     , factor: Maybe Int
--     }


type FragmentData
    = NoData
    | Mode UiMode
    | SizeRule SizeRule
    | ModeAndSizeRule UiMode SizeRule


applyFragment : Fragment -> Model -> Model
applyFragment fragment model =
    case decodeFragment fragment of
        ModeAndSizeRule mode rule ->
            { model
            | size = rule
            , mode = mode
            }
        SizeRule rule ->
            { model
            | size = rule
            }
        Mode mode ->
            { model
            | mode = mode
            }
        NoData -> model


fragmentToMessage : Fragment -> Msg
fragmentToMessage fragment =
    case decodeFragment fragment of
        ModeAndSizeRule mode rule -> ChangeModeAndResize mode rule
        SizeRule rule -> Resize rule
        Mode mode -> ChangeMode mode
        NoData -> NoOp


applyUrl : Url -> Model -> Model
applyUrl url model =
    case url.fragment of
        Just fragment -> model |> applyFragment fragment
        Nothing -> model


prepareUrlFragment : Model -> Fragment
prepareUrlFragment model =
    ModeAndSizeRule model.mode model.size
        |> encodeFragment


onUrlChange : Url -> Msg
onUrlChange url =
    case url.fragment of
        Just fragment -> fragmentToMessage fragment
        Nothing -> NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest req = NoOp


decodeFragment : String -> FragmentData
decodeFragment str =
    case String.split "/" str of
        modeStr::ruleStr::_ -> ModeAndSizeRule (decodeMode modeStr) (decodeSizeRule ruleStr)
        modeOrRule::_ ->
            case tryDecodingMode modeOrRule of
                Ok mode ->
                    Mode mode
                Err sizeRuleStr ->
                    SizeRule <| decodeSizeRule sizeRuleStr
        _ -> NoData


encodeFragment : FragmentData -> String
encodeFragment data =
    case data of
        ModeAndSizeRule mode rule ->
            encodeMode mode ++ "/"
                ++ encodeSizeRule rule
        Mode mode ->
            encodeMode mode
        SizeRule rule ->
            encodeSizeRule rule
        NoData -> ""
