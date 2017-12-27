module Update exposing (..)
import Msgs exposing (..)
import Models exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model | fps = floor (1000 / dt), theta = model.theta +  dt / 4000 }
            , Cmd.none
            )

        AdjustVertices verticesCount ->
            ( { model
              | numVertices = verticesCount
              , lorenz = model.config
                |> lorenz model.numVertices }
            , Cmd.none
            )
        ChangeConfig newConfig ->
            ( { model
              | config = newConfig
              , lorenz = newConfig
                |> lorenz model.numVertices
              }
            , Cmd.none
            )
        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )
        _ -> ( model, Cmd.none )
