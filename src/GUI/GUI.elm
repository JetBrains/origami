module GUI exposing (..)


type Cell
    = Knob Float
    | Toggle Bool
    | Button (() -> ())
    | Expand Bool (List Cell)
    | Select Cell (List Cell)
    -- | Color


type UI = List Cell



