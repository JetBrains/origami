module ImportExport exposing
    ( encodeModel
    , decodeModel
    , Model
    , Layer
    , EncodedState
    , defaultLayer
    )

import Time exposing (Time)

import Json.Decode as D exposing (int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (decode, required, optional, hardcoded)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import Layer.FSS as FSS

import Blend as Blend exposing (Blend)

type alias EncodedState = String

type alias Layer =
    { type_ : String
    , blend : Blend
    , config :  FSS.Config
    , mesh : FSS.Mesh
    }


type alias Model =
    { theta : Float
    , layers : List Layer
    , size : (Int, Int)
    , mouse : (Int, Int)
    , time : Time
    }


encodeIntPair : ( Int, Int ) -> E.Value
encodeIntPair ( v1, v2 ) =
    E.object
        [ ( "v1", E.int v1 )
        , ( "v2", E.int v2 )
        ]


encodeLayer : Layer -> E.Value
encodeLayer layer =
    E.object
        [ ( "type", E.string layer.type_ )
        , ( "blend", Blend.encodeOne layer.blend |> E.string )
        , ( "config", E.string "" )
        , ( "mesh", E.string "" )
        ]


encodeModel_ : Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "theta", E.float model.theta )
        , ( "layers", E.list (List.map encodeLayer model.layers) )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        , ( "size", encodeIntPair model.size )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "time", E.float model.time )
        ]


encodeModel : Model -> EncodedState
encodeModel model = model |> encodeModel_ |> E.encode 2


defaultLayer : Layer
defaultLayer =
    { type_ = "empty"
    , blend = Blend.default
    , config = FSS.init
    , mesh = FSS.emptyMesh
    }


layersDecoder : D.Decoder (List Layer)
layersDecoder =
    let
        createLayer type_ blend config mesh =
            { type_ = type_
            , blend = Blend.decodeOne blend
                |> Maybe.withDefault Blend.default
            , config = FSS.init
            , mesh = FSS.emptyMesh
            }
    in
        D.list
            ( D.decode createLayer
                |> D.required "type" D.string
                |> D.required "blend" D.string
                |> D.required "config" D.string
                |> D.required "mesh" D.string
            )


intPairDecoder : D.Decoder (Int, Int)
intPairDecoder =
    D.decode (\i1 i2 -> (i1, i2))
        |> D.required "v1" D.int
        |> D.required "v2" D.int


modelDecoder : D.Decoder Model
modelDecoder =
    D.decode Model
        |> D.required "theta" D.float
        |> D.required "layers" layersDecoder
        |> D.required "size" intPairDecoder
        |> D.required "mouse" intPairDecoder
        |> D.required "time" D.float


decodeModel : EncodedState -> (Model -> a) -> Maybe a
decodeModel modelStr f =
    D.decodeString modelDecoder modelStr
        |> Result.toMaybe
        |> Maybe.map f
