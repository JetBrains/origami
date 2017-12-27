port module Ports exposing (..)
import Configs exposing (..)
import Msgs exposing (..)


port pause : (() -> msg) -> Sub msg

port start : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port modify : (LorenzConfig -> msg) -> Sub msg