import GraphicSVG exposing (..)
import Array

-- don't worry about this, this type wraps up information needed for making games and animation

type Message = GameTick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.
              | NextSlide
              | LastSlide


-- this is the main function, and for simple animations, you would only replace the view function, or edit it below

main = gameApp GameTick {
                            model = model
                        ,   view = view
                        ,   update = update
                        }

-- MODEL

model = {
              t = 0 ,
            idx = 0 
        }

-- VIEW

view model = let t = model.t 
                 slide = Maybe.withDefault default (Array.get model.idx slides)

             in collage 1000 500 (slide t ++ borders ++ detectors)

-- UPDATE

update message model =
  case message of
    GameTick tick (getKeyState,changeP1,changeP2) -> { model |
                                       t = model.t + 4
                              }
    NextSlide -> { model |
    t   = 0 ,
    idx = min (model.idx + 1) (Array.length slides - 1) 
  }
    LastSlide -> { model |
    t   = 0 ,
    idx = max (model.idx - 1) 0
  }

--- MISCELLANEOUS

default t = []

borders = [rect 5000 5000
              |> filled white
              |> move (3000,0),
           rect 5000 5000
              |> filled white
              |> move (-3000,0),
           rect 5000 5000
              |> filled white
              |> move (0,2750),
           rect 5000 5000
              |> filled white
              |> move (0,-2750)]

detectors = [rect 5000 5000
              |> filled white
              |> makeTransparent 0
              |> move (2500,0)
              |> notifyTap NextSlide,
             rect 5000 5000
              |> filled black
              |> makeTransparent 0
              |> move (-2500,0)
              |> notifyTap LastSlide]


-- FUNCTIONS

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

---



-- Down here is where you will find the slides!
-- To add more slides, simply add them to the list below.

slides = Array.fromList [slide1,slide2]

slide1 t = [ text "Your cool title here"
                |> size 50
                |> centered
                |> filled black
                |> move (0,20),
             text "Enter name here" 
                |> size 20
                |> centered
                |> filled black
                |> move (0,-20)]

slide2 t = [fireball t]

-- My beautiful Mac Eng Fireball

fireball t = group [
  flames
    |> filled (glowing t)
    |> move (60,0)
  ,
  wedge 55 0.5
    |> filled (glowing t)
    |> rotate (degrees -90)
    |> move (10,5)
  ,
  group [
  curly t
    |> outlined (solid 13) white
  ,
  circle 8
    |> filled white
    |> move (5,0)
  ,
  tail 
   ] |> scale (0.93) |> move (10,10)
  ]

tail = curve (0,0) [Pull (20,8) (30,33),
                    Pull (32,30) (35,30),
               Pull (22,-6) (5,-11.5)]
    |> filled white
    |> move (18,-42)

-- You see this here

flames = curve (0,0)  [ Pull (10,-8) (20,0),
                        Pull (10,0) (10,10),
                        Pull (10,15) (10,20),
                        Pull (10,28) (0,24),
                        Pull (15,30) (8,47),
                        Pull (10,60) (18,56),
                        Pull (8,72) (-10,50),
                        Pull (-5,63) (-16,78),
                        Pull (-22,90) (-10,98),
                        Pull (-35,100) (-36,71),
                        Pull (-42,95) (-63,100),
                        Pull (-55,95) (-59,70),
                        Pull (-65,80) (-90,75),
                        Pull (-100,78) (-100,85),
                        Pull (-110,70) (-83,55),
                        Pull (-100,55) (-105,50),
                        Pull (-125,33) (-130,50),
                        Pull (-130,20) (-105,20),
                        Pull (-110,20) (-115,10),
                        Pull (-120,-10) (-130,0),
                        Pull (-120,-20) (-105,-10)]

-- Drove me insane

curly t = openPolygon (List.map getPoint [10..130])

getPoint t = (getY (t/10), getX (t/10))

getY t = (4*t) * sin(t)
getX t = -(4*t) * cos(t)

maroon = rgb 128 0 0

glowing t = let r = 218 - 90*abs(cos (degrees t))
                g = 100 - 100*abs(cos (degrees t))
                b = 0
            in rgb r g b    

-- © Ray Winardi 2016, in memory of 3 hours of his life
