module Gui.Util exposing (..)


import Gui.Def exposing (..)
import Gui.Mouse exposing (..)


applyMove : MouseState -> MouseState -> KnobState -> Float -> AlterKnob
applyMove prev next curState curValue =
    case next.dragFrom of
        Just dragFrom ->
            if next.pos /= dragFrom then
                let
                    originY = dragFrom.y
                    curY = next.pos.y
                    bottomY = toFloat originY - (knobDistance / 2)
                    topY = toFloat originY + (knobDistance / 2)
                    -- Y is going from top to bottom
                    -- diffY = (toFloat curY - bottomY) / knobDistance
                    diffY = (topY - toFloat curY) / knobDistance
                in
                    Alter
                        <| if diffY > 1 then 1.0
                            else if diffY < 0 then 0.0
                                else diffY
            else Stay
        _ -> Stay
    -- let
    --     ( prevX, prevY ) = prev.vec
    --     ( nextX, nextY ) = next.vec
    -- in
    --     if (nextY == 0.0) then Stay
    --         else if (nextY < 0.0) then Down
    --             else if (nextY > 0.0) then Up
    --                 else Stay


alterKnob : KnobState -> AlterKnob -> Float -> Float
alterKnob { min, max, step } alter curValue =
    case alter of
        Alter amount ->
            -- amount is a (0 <= value <= 1)
            amount * (max - min) -- TODO: apply step
        Stay -> curValue
