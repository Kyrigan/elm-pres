import GraphicSVG exposing (..)
import Array

type Message = GameTick Float GetKeyState --The tick needs to have Float and GetKeyState which handles key presses.
              | NextSlide
              | LastSlide

-- this is the main function, and for simple animations, you would only replace the view function, or edit it below

main = gameApp GameTick {
                            model = init
                        ,   view = view
                        ,   update = update
                        }

-- MODEL

init = {
              t = 0 ,
            idx = 0 ,
              p = False, -- Pause
              r = 1 , -- Rewind
              a = 1  -- Acceleration
        }

-- VIEW

view model = let t = model.t 
                 slide = Maybe.withDefault default (Array.get model.idx slides)

             in collage 1000 500 (slide t ++ borders ++ detectors)

-- UPDATE

update message model =
  case message of
    GameTick tick (getKeyState,changeP1,changeP2) -> 
                              if (getKeyState LeftArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = max (model.idx - 1) 0
                              }
                              else if (getKeyState RightArrow) == JustDown then
                              { model |
                                  t   = 0 ,
                                  idx = min (model.idx + 1) (Array.length slides - 1) 
                              }
                              else if (getKeyState Space) == JustDown then
                              { model |
                                  p = not model.p
                              }
                              else if (getKeyState UpArrow) == JustDown then
                              { model |
                                  a = min (model.a * 2) 4
                              }
                              else if (getKeyState DownArrow) == JustDown then
                              { model |
                                  a = max (model.a / 2) 0.5
                              }
                              else if (getKeyState (Key "R")) == JustDown then
                              { model |
                                  r = -model.r
                              }
                              else if (getKeyState Backspace) == JustDown then
                              { model |
                                  t = 0
                              }
                              else if model.p then
                              model
                              else
                              { model |
                                       t = max (model.t + 2.5 * model.a * model.r) 0
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

detectors = [ group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                      ] |> move (450,-200)
                        |> makeTransparent 0.5
                |> notifyTap NextSlide
              ,
              group [ circle 40
                        |> filled gray
                      ,
                      triangle 30
                        |> filled white

                    ] |> rotate (degrees 180)
                      |> move (-450,-200)
                      |> makeTransparent 0.5
                |> notifyTap LastSlide
            ]


-- FUNCTIONS

--<< So why do I see (t - 100) or whatever value so often? >>

--   Whenever I do that, I'm basically delaying what I want to happen
--   by that value. Is it measure in seconds, frames or what? What's the unit here?
--   To be honest, I don't know. It has a lot to do with the UPDATE function, and 
--   what value for 'x' you are using for " t = model.t + x ".

disappear x n = if x > n then makeTransparent 0 else makeTransparent 1 -- Makes things vanish off the screen! 
 
loop t n = let y = toFloat (floor (t / n)) -- This function is how I make things loop!
           in t - y * n                                           

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

drawLine t (x1,y1) (x2,y2) = line (x1,y1) (x1 + tranSin (t) (x2 - x1), y1 + tranSin (t) (y2 - y1))

-- Down here is where you will find the slides!
-- To add more slides, simply add them to the list below.

slides = Array.fromList [slide1,slide2,slide3]

--<< EVERYTHING FOR SLIDE 1 ( EXCEPT FIREBALL ) >>-

slide1 t = [ 
             text "Computer Science Outreach"
                |> size 50
                |> customFont "Helvetica"
                |> bold
                |> centered
                |> outlined (solid 2) maroon
                |> move (0,-25),
             text "Every Thursday in ETB 126 starting 6:30 P.M." 
                |> size 16
                |> customFont "Helvetica"
                |> centered
                |> filled darkRed
                |> move (0,-90)
                |> fadeIn t 800
                ,
            fireball
                |> move (0,100),
            wifi (loop t  500)
                |> move (80,120)
                |> rotate (degrees -90)
                |> fadeIn t 500 ,
            circle 100
                |> filled white
                |> move (10,110)
                |> scale (1 - tranSin (t-450) 1),
              rect 1000 500
                |> filled blue
                |> move (500 + (tranSin (t-300) 510),0)
                ,
              rect 1000 500
                |> filled blue
                |> move (-500 - (tranSin (t-300) 510),0),
              text "RAY"
                |> size 50
                |> customFont "Helvetica"
                |> bold
                |> filled white
                |> move (-480,190)
                |> appear t 100
               ,
              text "PRESENTS"
                |> size 50
                |> customFont "Helvetica"
                |> bold
                |> filled white
                |> move (-480,140)
                |> appear t 200
                ]

wifi t = group [ 
               curve (-20,10) [Pull (0,35) (20,10)]
                  |> outlined (solid 8) maroon
                  |> fadeIn t 100 
              ,   
               curve (-35,20) [Pull (0,60) (35,20)]
                  |> outlined (solid 8) maroon
                  |> fadeIn t 200 
              ,   
               curve (-50,30) [Pull (0,85) (50,30)]
                  |> outlined (solid 8) maroon
                  |> fadeIn t 300 
          ] |> fadeOut t 400 

--<< SLIDE 2 >>--

slide2 t = [ text "Percentage of New STEM Jobs By Area Through 2018"
                |> size 24
                |> customFont "Helvetica"
                |> bold
                |> centered
                |> filled black
                |> move (0,190)
                |> fadeIn t 100
               ,
             drawLine (t-950) (-100,99) (100,59)
                |> outlined (dashed 1) black
             ,
             drawLine (t-950) (-100,-99) (100,-59)
                |> outlined (dashed 1) black
             ,
             --<< FIRST PIECHART>>-- 
             piechart [(0.07,optRed),
                       (0.16,optBlue),
                       (0.02,optGreen),
                       (0.04,optPurple),
                       (0.71,optOrange)] 0 
                |> scale (tranSin (t-200) 1)
                |> move (-100,0)
                |> rotate (degrees 120)
             , 
             legend (t-350)  [("Physical Sciences - 7%",red   ,1),
                        ("Traditional Engineering - 16%",blue  ,2),
                        ("Mathematics - 2%",green   ,3),
                        ("Life Sciences - 4%",purple ,4),
                        ("Computing - 71%",orange ,5)]
                 |> move (-385,75)
             ,
             --<< SECOND PIECHART>>--             
             piechart [(0.27/0.71,optRed),
                       (0.07/0.71,optBlue),
                       (0.02/0.71,optGreen),
                       (0.10/0.71,optPurple),
                       (0.21/0.71,optOrange),
                       (0.03/0.71,optGold),
                       (0.01/0.71,optGray)] 0 
                |> scale (tranSin (t-900) 0.6)
                |> move (100,0)
                |> rotate (degrees 120)
             ,
             legend (t-1000) [("Software Engineering - 27%",red   ,1),
                             ("Computer Support - 7%",blue  ,2),
                             ("Database Admin - 2%",green   ,3),
                             ("System Analysis - 10%",purple ,4),
                             ("Computer Networking - 21%",orange ,5),
                             ("Other Computing - 3%",optGold ,6),
                             ("CS/IS Research - 1%",optGray ,7)]
                |> move (215,100) 
            ]

optRed = rgb 211 94 95
optBlue = rgb 114 147 203
optGreen = rgb 132 186 91
optOrange = rgb 225 151 76
optPurple = rgb 144 103 167
optGold = rgb 204 194 16
optGray = rgb 128 133 133

piechart data start = group (createPie data start)

createPie data start = case data of 
              (x,c) :: xs -> (wedge 100 x 
                                |> filled c
                                |> rotate (degrees (start + 180 * x))
                                ) :: createPie xs (start + 360 * x)
              _ -> []

legend t l =  
                   group (List.map (makeLabels t) l)
                                   

makeLabels t (label,c,n) = group [ square 8
                                    |> filled c
                                    |> move (-15,4)
                                     ,
                                  text label
                                    |> size 12
                                    |> customFont "Helvetica"
                                    |> filled c] 
                                        |> move (0,-25 * n)
                                        |> fadeIn t (n * 100)

--<< SLIDE 3 >>--

slide3 t = [text "What exciting experiences await you?"
                |> size 24
                |> customFont "Helvetica"
                |> bold
                |> centered
                |> filled black
                |> move (0,190)
                |> fadeIn t 100
               ,
            group( makeBullets (t-100) 
                          ["Cripling depression",
                           "Stress-induced panic attacks",
                           "Potential risk for substance abuse",
                           "Existential crisis",
                           "Selling your soul to Apple"] 0)
                |> move (110,100)
              ,
             butHonestly t |> appear t 1000
           ]

butHonestly t = group [rect 1200 600
                            |> filled darkGreen,
                       makeItRain (t - 1000)
                       ,
                       makeItRain (1000 - t)
                       ,
                       text "WHO CARES?"
                          |> size 80
                          |> customFont "Helvetica"
                          |> bold
                          |> centered
                          |> filled white
                          |> move (0,60) ,
                       text "You're looking at a $90,000 salary!"
                          |> size 40
                          |> customFont "Helvetica"
                          |> bold
                          |> centered
                          |> filled white
                          |> move (0,-20)
                          |> fadeIn t 1100
                         ]

makeBill t n = 
  let 
    x = (n * 2) * cos (degrees (n * 10))
    y = (n * 2) * sin (degrees (n * 10))

  in
    group [ 
            text "$"
              |> size 36
              |> customFont "Helvetica"
              |> bold
              |> centered
              |> filled billGreen
              |> move (x,y)
              |> fadeIn (loop (t - n * 5) 300) 0
              |> fadeOut (loop (t - n * 5 + 100) 300) 0
               ]

billGreen = rgb 133 187 101

makeItRain t = group(List.map (makeBill t) [0..300])

-- This is just for AUTOMATIC bullets. You are free to make
-- more customized bullets, but they will take a bit longer.

makeBullets t l start = case l of
  x::xs -> group [
            text x
              |> size 20
              |> customFont "Helvetica"
              |> filled black
              |> move (-200,start)
              |> fadeIn t 100
            ,
            circle 5
              |> filled black
              |> move (-220,start+5)
              |> fadeIn t 100
            ] :: makeBullets (t-100) xs (start - 35)
  _     -> []

-- My beautiful Mac Eng Fireball

fireball = group [
  flames
    |> filled maroon
    |> move (60,0)
  ,
  wedge 55 0.5
    |> filled maroon
    |> rotate (degrees -90)
    |> move (10,5)
  ,
  group [
  curly
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

curly = openPolygon (List.map getPoint [10..130])

getPoint t= (getY (t/10), getX (t/10))

getY t = (4*t) * sin(t)
getX t = -(4*t) * cos(t)

maroon = rgb 128 0 0



-- © Ray Winardi 2016, in memory of 3 hours of his life
