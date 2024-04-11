module Gui.Mouse exposing (..)


type alias Position = { x: Int, y : Int }


type alias MouseState =
    { pos: Position
    , down : Bool
    , dragFrom : Maybe Position
    }


moves : Position -> MouseState -> MouseState
moves pos prev =
    { prev
    | pos = pos
    }


ups : a -> MouseState -> MouseState
ups _ prev =
    { prev
    | down = False
    , dragFrom = Nothing
    }


downs : Position -> MouseState -> MouseState
downs pos prev =
    { prev
    | pos = pos
    , down = True
    , dragFrom =
        case prev.dragFrom of
            Just prevDragPos -> Just prevDragPos
            Nothing -> Just pos
    }


init : MouseState
init =
    { pos = { x=0, y=0 }
    , down = False
    , dragFrom = Nothing
    }
