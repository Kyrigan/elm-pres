import GraphicSVG exposing (..)
import Array


type Message = GameTick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.

-- this is the main function, and for simple animations, you would only replace the view function, or edit it below

main = gameApp GameTick {
                            model = model
                        ,   view = view
                        ,   update = update
                        }

-- MODEL

model = {
              t = 0
        }

-- VIEW

view model = let t = model.t 
             in collage 1000 500 []

-- UPDATE

update message model =
  case message of
    GameTick tick (getKeyState,changeP1,changeP2) -> { model |
                                       t = model.t + 4 }

-- FUNCTIONS

loop t n = let y = toFloat (floor (t / n)) -- This function is how I make things loop!
           in t - y * n   

disappear x n = if x > n then makeTransparent 0 else makeTransparent 1 -- Makes things vanish off the screen! 
 
mod x n = let y = toFloat (floor (x / n)) -- This function is how I make things loop!
          in x - y * n                                        

appear x n =    if x > n then makeTransparent 1 else makeTransparent 0 -- Makes things suddenly appear on the screen!
                                          
fadeIn t n = makeTransparent (tranSin (t-n) 1) 

fadeOut t n = makeTransparent (1 - (tranSin (t-n) 1)) 

trans t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) LINEAR.
               then 0 
            else Basics.min t y

tranSin t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
               then 0 
            else if t/100 > pi/2 then y
            else sin(t/100) * y

