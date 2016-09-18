import GraphicSVG exposing (..)
import Html.App as Html
import AnimationFrame exposing (..)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Platform.Sub as Sub
import Array

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- FUNCTIONS

disappear x n = if x > n then makeTransparent 0 else makeTransparent 1 -- Makes things vanish off the screen! 
 
loop t n = let y = toFloat (floor (t / n)) -- This function is how I make things loop!
           in t - y * n                                        

appear x n =    if x > n then makeTransparent 1 else makeTransparent 0 -- Makes things suddenly appear on the screen!
                                          
fadeIn t n s = makeTransparent (tranSin (t-n) 1 1) 

fadeOut t n s = makeTransparent (1 - (tranSin (t-n) 1 1)) 

tranSin t y s = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.) Uses sin.
               then 0 
            else if s*t/100 > pi/2 then y
            else sin(s*t/100) * y

moveRight x t off s = move (tranSin (t-off) x s,0)

moveLeft x t off s  = move (tranSin (t-off) -x s,0)

moveUp   x t off s = move (0,tranSin (t-off) x s)

moveDown x t off s = move (0,tranSin (t-off) -x s)

vibrate t off mag n = -- Vibrates left and right. 'off' for time offset, 
                    -- 'mag' for magnitude of vibration, 'n' for number of vibrations
              if t > off && (t-off)/4 < 2*n*pi then 
                 move(mag*sin((t-off)/4),0) 
                 else move(0,0)
             
expand t off s = scale (tranSin (t-off) 1 s)

shrink t off s = scale (1 - tranSin (t-off) 1 s)

-- MODEL


type alias Model = {t : Time, 
                count : Int , 
                    a : Float, -- Acceleration  
                    r : Float, -- Rewind
                    pause : Bool        }


init : (Model, Cmd Msg)
init =
  ( reset
    , 
   Cmd.none)

reset = Model 0 0 1 1 False

-- UPDATE


type Msg
  = Tick Time | ChangeSlide KeyCode | Forward | Back


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick dt -> if model.pause then (model,Cmd.none) else 
                    if model.t <= 0 && model.r == -1 then (model,Cmd.none) else
                  ({model | t = model.t + dt * model.a/4 * model.r}, Cmd.none)
    ChangeSlide n -> (model |> changeSlide n, Cmd.none)
    Forward -> ({model | count = Basics.min (model.count + 1) (Array.length slides - 1), t = 0 , r = 1}
                , Cmd.none )
    Back    -> ({model | count = Basics.max (model.count - 1) 0 , t = 0 , r = 1}
                , Cmd.none )

changeSlide n model = case n of
                      39 -> {model | count = Basics.min (model.count + 1) (Array.length slides - 1), t = 0 , r = 1}
                      37 -> {model | count = Basics.max (model.count - 1) 0 , t = 0 , r = 1}
                      27 -> reset
                      32 -> {model | t = 0, r = 1} -- Replays the current slide. Press 'Space'
                      65 -> {model | a = accelerate model.a}  -- Secret cheat code! Speeds through the slide. Press 'A'
                      80 -> {model | pause = not model.pause} -- Pause and unpause :) Press 'P'
                      82 -> {model | r = -model.r}
                      _  -> model


accelerate a = case a of
                        1 -> 2
                        2 -> 3
                        3 -> 0.5
                        _ -> 1

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [diffs Tick,
             Keyboard.downs ChangeSlide]



-- VIEW

view model = let t = model.t 
                 slide = Maybe.withDefault default (Array.get model.count slides)

             in collage 1000 500 (slide t ++ borders ++ detectors)
              
slides = Array.fromList [opening,closing]

opening t = [ text "Once upon a time..."
                |> size 100
                |> filled black
                |> move (-400,0) ]



closing t = [fireball t]

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

flames = curve (0,0) [Pull (10,-8) (20,0),
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

curly t = openPolygon (List.map getPoint [10..130])

getPoint t = (getY (t/10), getX (t/10))

getY t = (4*t) * sin(t)
getX t = -(4*t) * cos(t)

maroon = rgb 128 0 0

glowing t = let r = 218 - 90*abs(cos (degrees t))
                g = 100 - 100*abs(cos (degrees t))
                b = 0
            in rgb r g b    

------------

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
              |> notifyTap Forward,
             rect 5000 5000
              |> filled black
              |> makeTransparent 0
              |> move (-2500,0)
              |> notifyTap Back]

