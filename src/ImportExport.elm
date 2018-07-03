module ImportExport exposing (decodeModel, EncodedState)

import Time exposing (Time)

import Json.Decode as D exposing (int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (decode, required, optional, hardcoded)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import Layer.FSS as FSS

import Blend exposing (Blend)

type alias EncodedState = String

type alias Layer =
    { type_ : String
    , blend : Blend
    , config :  FSS.Config
    , mesh : FSS.Mesh
    }

type alias Model =
    { theta : Float
    , layers : List (Maybe Layer)
    , size : (Int, Int)
    , mouse : (Int, Int)
    , time : Time
    }

decodeLayers : D.Decoder (List (Maybe Layer))
decodeLayers =
    let
        createLayer type_ blend config mesh =
            case type_ of
                "fss" -> Nothing -- TODO
                _ -> Nothing
    in
        D.list
            ( D.decode createLayer
                |> D.required "type" D.string
                |> D.required "blend" D.string
                |> D.required "config" D.string
                |> D.required "mesh" D.string
            )


decodeIntPair : D.Decoder (Int, Int)
decodeIntPair =
    D.decode (\i1 i2 -> (i1, i2))
        |> D.required "v1" D.int
        |> D.required "v2" D.int

decodeModel : D.Decoder Model
decodeModel =
    D.decode Model
        |> D.required "theta" D.float
        |> D.required "layers" decodeLayers
        |> D.required "size" decodeIntPair
        |> D.required "mouse" decodeIntPair
        |> D.required "time" D.float
