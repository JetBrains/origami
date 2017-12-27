module Msgs exposing (..)
import Time exposing (Time)
import Configs exposing (..)
import Window

type Msg
    = Animate Time
    | Resize Window.Size
    | ChangeConfig LorenzConfig
    | AdjustVertices Int
    | Rotate Float
    | Pause
    | Start
