port module Main exposing (main)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (width, height, style, class)
import AnimationFrame
import Time exposing (Time)
import Window
import Task exposing (Task)
import WebGL exposing (Mesh, Option)
import WebGL.Settings exposing (sampleAlphaToCoverage)
import WebGL.Settings.DepthTest as DepthTest

import Viewport exposing (Viewport)

import Layer.FSS as FSS


type FSS = FSS FSS.Config (Maybe FSS.SerializedScene) FSS.Mesh


type alias Model =
    { paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , fss : FSS
    , size : ( Int, Int )
    , now : Time
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | RebuildFss FSS.SerializedScene
    | ConfigureFss FSS.Config
    | Rotate Float
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        fssConfig = FSS.init
    in
        (
            { paused = False
            , autoRotate = False
            , fps = 0
            , theta = 0.1
            , fss =
                FSS fssConfig Nothing (FSS.build fssConfig Nothing)
            , size = ( 1500, 800 )
            , now = 0.0
            }
        , Cmd.batch
            [ Task.perform Resize Window.size
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
             (
                { model
                 | fps = floor (1000 / dt)
                 , theta = if not model.autoRotate then model.theta + dt / 4000 else model.theta
                 , now = if not model.paused then model.now + dt else model.now
                 }
             , Cmd.none
             )

        Resize { width, height } ->
            ( model
            , Cmd.none
            )

        RebuildFss serializedScene ->
            (
                { model
                | fss =
                    case model.fss of
                        FSS cfg _ mesh ->
                            let
                                maybeScene = Just serializedScene
                                newMesh = maybeScene |> FSS.build cfg
                            in
                                FSS cfg maybeScene newMesh
                }
            , Cmd.none
            )

        ConfigureFss newConfig ->
            (
                { model
                | fss =
                    case model.fss of
                        FSS _ maybeScene mesh ->
                            FSS newConfig maybeScene mesh
                }
            , Cmd.none
            )

        Pause ->
          ( { model | paused = not model.paused }
          , Cmd.none
          )

        _ -> ( model, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        , changeFss (\newConfig ->
            ConfigureFss newConfig
        )
        , receiveFss (\serializedMesh ->
            RebuildFss serializedMesh
        )
        , pause (\_ -> Pause)
        , start (\_ -> Start)
        ]


view : Model -> Html Msg
view { theta, size, now, fss } =
    let
        viewport = Viewport.find { theta = theta, size = size }
    in div [ ]
        (
        --span [ class "fps" ] [ toString model.fps ++ "FPS" |> text ]
        --    :: Html.map mapControls
        --     (config |>
        --           Controls.controls numVertices theta)
        --:: WebGL.toHtmlWith
        WebGL.toHtmlWith
            [ WebGL.antialias
            , WebGL.alpha True
            , WebGL.clearColor 0.0862745098 0.0862745098 0.0862745098 1.0
            --, WebGL.depth 0.5
            ]
            [ width (Tuple.first size)
            , height (Tuple.second size)
            , style [ ( "display", "block" ), ("background-color", "#161616") ]
            ]
            [ case fss of
                FSS _ fssSerialized fssMesh ->
                    FSS.makeEntity
                        viewport
                        now
                        fssSerialized
                        -- [ DepthTest.default, Blend.produce blend, sampleAlphaToCoverage ]
                        [ DepthTest.default, sampleAlphaToCoverage ]
                        fssMesh
            ]
        :: []
        )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port pause : (() -> msg) -> Sub msg

port start : (() -> msg) -> Sub msg

port changeFss : (FSS.Config -> msg) -> Sub msg

port receiveFss : (FSS.SerializedScene -> msg) -> Sub msg
