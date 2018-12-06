module Gui.Mouse exposing (..)


type alias MouseState =
    { pos: (Int, Int)
    , vec: (Float, Float)
    , down : Bool
    }


moves : ( Int, Int ) -> MouseState -> MouseState
moves pos prev =
    { prev
    | vec = findMouseVec prev.pos pos
    , pos = pos
    }


ups : a -> MouseState -> MouseState
ups _ prev =
    { prev
    | down = False
    }


downs : a -> MouseState -> MouseState
downs _ prev =
    { prev
    | down = True
    }


init : MouseState
init =
    { pos = ( 0, 0 )
    , vec = ( 0, 0 )
    , down = False
    }


--withMouseAt : Pos -> MouseState
withMouseAt pos =
    { pos = pos
    , vec = ( 0, 0 )
    , down = False
    }


--findMouseVec : Pos -> Pos -> ( Float, Float )
findMouseVec ( prevX, prevY ) ( curX, curY ) =
    ( if prevX == curX then 0.0
        else if prevX < curX then -1.0
            else 1.0
    , if prevY == curY then 0.0
        else if prevY < curY then -1.0
            else 1.0
    )
