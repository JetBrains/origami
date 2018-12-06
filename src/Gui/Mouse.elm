module Gui.Mouse exposing (..)


type alias MouseState =
    { pos: (Int, Int)
    , down : Bool
    , dragFrom : Maybe (Int, Int)
    , vec: Maybe (Float, Float)
    }


moves : ( Int, Int ) -> MouseState -> MouseState
moves pos prev =
    { prev
    | vec =
        prev.dragFrom
            |> Maybe.map (\dragStart ->
                findMouseVec dragStart pos)
    , pos = pos
    }


ups : a -> MouseState -> MouseState
ups _ prev =
    { prev
    | down = False
    , vec = Nothing
    , dragFrom = Nothing
    }


downs : ( Int, Int ) -> MouseState -> MouseState
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
    { pos = ( 0, 0 )
    , down = False
    , vec = Nothing
    , dragFrom = Nothing
    }


--findMouseVec : Pos -> Pos -> ( Float, Float )
findMouseVec ( originX, originY ) ( curX, curY ) =
    ( if originX == curX then 0.0
        else if originX < curX then -1.0
            else 1.0
    , if originY == curY then 0.0
        else if originY < curY then -1.0
            else 1.0
    )
