port module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html, text, div, span)
import Html.Attributes exposing (width, height, style, class)
import AnimationFrame
import Time exposing (Time)
import Window
import Task exposing (Task)
import WebGL exposing (Mesh)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.DepthTest as DepthTest

import Viewport exposing (Viewport)
import Blend exposing (Blend)
import Controls

import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.FSS as FSS
import Layer.Template as Template


type alias LayerIndex = Int


type LayerKind
    = Lorenz
    | Fractal
    | Voronoi
    | FSS
    | Template


type LayerConfig cfg = LayerConfig LayerKind cfg


type Layer mesh = Layer (LayerKind -> Blend -> mesh)


type MeshBuilder mesh cfg = MeshBuilder (cfg -> mesh)


type EntityMaker mesh = EntityMaker (Viewport {} -> List Setting -> mesh -> WebGL.Entity)


type alias Model =
    { paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , layers : Array Layer
    , size : ( Int, Int )
    }


type Msg
    = Animate Time
    | Resize Window.Size
    | Configure LayerIndex LayerConfig
    | ChangeBlend LayerIndex Blend
    | RebuildFss FSS.SerializedMesh
    | Rotate Float
    | Pause
    | Start


init : ( Model, Cmd Msg )
init =
    let
        lorenzConfig = Lorenz.init
        fractalConfig = Fractal.init
        voronoiConfig = Voronoi.init
        templateConfig = Template.init
    in
        (
            { paused = False
            , autoRotate = True
            , fps = 0
            , theta = 0.1
            , layers = Array.fromList
                -- [ Layer Blend.default (fractalConfig |> Fractal.build)
                [ Layer Blend.default (voronoiConfig |> Voronoi.build)
                --, Layer Blend.default (lorenzConfig |> Lorenz.build)
                , Layer Blend.default (templateConfig |> Template.build)
                ]
            , size = ( 0, 0 )
            }
        , Cmd.batch
            [ Task.perform Resize Window.size
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( if model.autoRotate then
                { model
                | fps = floor (1000 / dt)
                , theta = model.theta + dt / 4000
                }
              else
                { model
                | fps = floor (1000 / dt)
                }
            , Cmd.none
            )

        Configure index config ->
            let
                curBlend = model.layers
                    |> Array.get index
                    |> Maybe.map getBlend
                    |> Maybe.withDefault Blend.default
                layer = case config of
                    LayerConfig kind config ->
                        Layer curBlend (config |> Lorenz.build)
            in
                ( { model
                | layers = model.layers
                    |> Array.set index layer
                }
                , Cmd.none
                )

        ChangeBlend index newBlend ->
            case model.layers |> Array.get index of
                Just layer ->
                    let
                        newLayer =
                            case layer of
                                Layer kind _ mesh -> Layer kind newBlend mesh
                    in
                        ( { model
                           | layers = model.layers
                               |> Array.set index newLayer
                           }
                           , Cmd.none
                           )
                Nothing ->
                    ( model , Cmd.none )

        Rotate theta ->
            ( { model | theta = theta  }
            , Cmd.none
            )

        Resize { width, height } ->
            ( { model | size = ( width, height ) }
            , Cmd.none
            )

        _ -> ( model, Cmd.none )


getBlend : Layer a -> Blend
getBlend layer =
    case layer of
        Layer _ blend _ -> blend


configureFirst : Model -> LayerConfig -> (LayerKind -> Bool) -> Msg
configureFirst { layers } config f =
    layers
        |> Array.foldl
            (\layer (lastIdx, indices) ->
                ( lastIdx + 1
                , case layer of
                    Layer kind _ _ ->
                        if f kind
                        then lastIdx :: indices
                        else indices
                )
            )
            (0, [])
        |> Tuple.second
        |> List.head
        |> Maybe.withDefault 0
        |> (\idx -> Configure idx config)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes Resize
        , rotate Rotate
        , changeBlend (\{ layer, blend } ->
            ChangeBlend layer blend
        )
        , modifyLorenz (\lorenzConfig ->
            configureFirst model (LayerConfig Lorenz lorenzConfig) (\kind ->
                kind == Lorenz
            )
        )
        , changeFss (\fssConfig ->
            configureFirst model (LayerConfig FSS fssConfig) (\kind ->
                kind == FSS
            )
        )
        , receiveFss (\serializedMesh ->
            RebuildFss serializedMesh
        )
        , pause (\_ -> Pause)
        , start (\_ -> Start)
        ]


mapControls : Model -> Controls.Msg -> Msg
mapControls model controlsMsg =
    case controlsMsg of
        Controls.Configure cfg -> Configure 0 (LayerConfig Lorenz cfg)
        Controls.Rotate th -> Rotate th


-- findBuilder kind =
--     case kind of
--         Voronoi -> MeshBuilder Voronoi.build
--         Lorenz -> MeshBuilder Lorenz.build
--         _ -> Template.build


findEntityMaker kind =
    case kind of
        Voronoi -> EntityMaker Voronoi.makeEntity
        Lorenz -> EntityMaker Lorenz.makeEntity
        _ -> EntityMaker Template.makeEntity


mergeLayers : Float -> Array Layer -> List WebGL.Entity
mergeLayers theta layers =
    let viewport = Viewport.find { theta = theta }
    in layers |> Array.map
        (\layer ->
            case layer of
                Layer kind blend mesh ->
                    let
                        (EntityMaker maker) = findEntityMaker kind
                    in
                        maker
                            viewport
                            [ DepthTest.default, Blend.produce blend ]
                            mesh
                -- LorenzLayer blend lorenz ->
                --     Lorenz.makeEntity
                --         viewport
                --         [ DepthTest.default, Blend.produce blend ]
                --         lorenz
                -- FractalLayer blend fractal ->
                --     Fractal.makeEntity
                --         viewport
                --         [ DepthTest.default, Blend.produce blend ]
                --         fractal
                -- TemplateLayer blend template ->
                --     Template.makeEntity
                --         viewport
                --         [ DepthTest.default, Blend.produce blend ]
                --         template
                -- VoronoiLayer blend voronoi ->
                --     Voronoi.makeEntity
                --         viewport
                --         [ DepthTest.default, Blend.produce blend ]
                --         voronoi
                -- FssLayer blend fss ->
                --     FSS.makeEntity
                --         viewport
                --         [ DepthTest.default, Blend.produce blend ]
                --         fss
                -- TextLayer blend ->
                --     -- FIXME: replace with text
                --     Template.makeEntity
                --         viewport
                --         [ DepthTest.default, Blend.produce blend ]
                --         (Template.init |> Template.build)
        )
    |> Array.toList


view : Model -> Html Msg
view model =
    div [ ]
        ( span [ class "fps" ] [ toString model.fps ++ "FPS" |> text ]
        --    :: Html.map mapControls
        --     (config |>
        --           Controls.controls numVertices theta)
           :: WebGL.toHtml
              [ width (Tuple.first model.size)
              , height (Tuple.second model.size)
              , style [ ( "display", "block" ), ("background-color", "#12181C") ]
              ]
              (model.layers |> mergeLayers model.theta)
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

port rotate : (Float -> msg) -> Sub msg

port modifyLorenz : (Lorenz.Config -> msg) -> Sub msg

-- TODO: port to affect camera

port changeFss : (FSS.Config -> msg) -> Sub msg

port receiveFss : (FSS.SerializedMesh -> msg) -> Sub msg

port changeBlend :
    ( { layer : Int
      , blend : Blend
      }
    -> msg) -> Sub msg
