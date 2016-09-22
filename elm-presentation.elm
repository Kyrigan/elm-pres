import GraphicSVG exposing (..)
import Array


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
                                       t = model.t + 4 }
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

---

slides = Array.fromList [slide1,slide2,slide3,slide4,slide5,slide6,slide7,slide8,slide9,slide10]

slide1 t = [    foodline t
              , pacman t
                    |> move (-650 + trans (t/2) 700,0)
                    |> scale (1 + tranSin (t-400) 0.3
                                + tranSin (t-700) 0.3
                                + tranSin (t-1000) 0.3
                                + tranSin (t-1300) 0.3
                                + tranSin (t-1400) 5)
              , title   
                    |> move (-500,0)
                    |> fadeIn t 1500
              , instructions 
                    |> move (-480,190)
                    |> fadeIn t 1500
                ]

foodline t = group [ facebook
                    |> move (-420,0)
                    |> disappear t 400
                ,youtube
                    |> move (-270,0)
                    |> disappear t 700
                ,linkedin
                    |> move (-110,0)
                    |> disappear t 1000
                ,tweet (rgb 94 169 208)
                    |> scale (0.7)
                    |> move (20,-20)
                    |> disappear t 1300
  ]

title = group [text "BIG"
                    |> size 200
                    |> bold
                    |> customFont "Helvetica"
                    |> filled white  
                    |> move (0,0)                 
                    
              , text "DATA" 
                    |> size 200
                    |> bold
                    |> customFont "Helvetica"
                    |> filled white 
                    |> move (0,-160)
                     
              , text "An Introduction by Ray Winardi" 
                    |> size 28              
                    |> customFont "Helvetica"
                    |> filled white 
                    |> move (12,-190)
                 ]

instructions = group [text "Use the left and right arrow keys to navigate through the slides."
                        |> size 14
                        |> customFont "Helvetica"
                        |> italic
                        |> filled white
                     ,text "Use the space key to replay a slide and the escape key to restart."
                        |> size 14
                        |> customFont "Helvetica"
                        |> italic
                        |> filled white
                        |> move(20,-18)]

vibrate t x n= -- Vibrates left and right
              if t > x && (t-x)/4 < 10*pi then 
                 move(n*sin((t-x)/4),0) 
                else move(0,0)
             




pacman t = group [wedge 100 0.51
                        |> filled yellow
                        |> rotate (Basics.degrees 
                                    (135 - ((abs(sin(t/90))))* 45)
                                    ),
                  wedge 100 0.51
                        |> filled yellow
                        |> rotate (Basics.degrees 
                                    (-135 + ((abs(sin(t/90))))* 45)
                                    )]

facebook = group [roundedRect 60 60 6
                        |> filled blue,
                  text "f"           
                        |> size 68
                        |> bold            
                        |> customFont "Verdana"
                        |> filled white
                        |> move (-14,-26)]

linkedin = group [roundedRect 60 60 6
                        |> filled darkBlue,
                  text "in"           
                        |> size 60
                        |> bold            
                        |> customFont "Arial"
                        |> filled white
                        |> move (-26,-22)]

youtube = group [roundedRect 70 48 9
                        |> filled (rgb 245 10 10)
                        ,
                 triangle 15
                        |> filled white
                        ]

grow t tLoop = (1 / tLoop) * (mod t tLoop)

slide2 t = [text "Let's look at the"
                  |> customFont "Helvetica"
                  |> bold
                  |> size 80
                  |> filled blue
                  |> move (-300,40)
                  |> fadeOut t 500,
            text "3 Vs of Big Data."
                  |> customFont "Helvetica"
                  |> bold
                  |> size 80
                  |> filled blue
                  |> move (-267,-40)
                  |> fadeOut t 500
            ,
            group [
            theV red
                  |> move (0,tranSin (t-500) 100)
                ,
            text "OLUME"
              |> customFont "Helvetica"
              |> bold
              |> size 80
              |> filled red
              |> move (-148,60)
              |> fadeIn t 600
      
                ] |> scale (1 + tranSin (t-700) 0.5 - tranSin (t-750) 0.5)
                 ,
            group [
            theV blue,text "ELOCITY"
              |> customFont "Helvetica"
              |> bold
              |> size 80
              |> filled blue
              |> move (-148,-40)
              |> fadeIn t 800
              ] |> vibrate t 900 14
              ,
              group [
            theV green
              |> move (0,tranSin (t-500) -100)
              ,                      
            text "ARIETY"
              |> customFont "Helvetica"
              |> bold
              |> size 80
              |> filled green
              |> move (-148,-140)
              |> fadeIn t 1000
                  ] |> rotate (Basics.degrees (tranSin (t-1200) 360))

              ]

theV clr = text "V"
            |> customFont "Helvetica"
            |> bold
            |> size 80
            |> filled clr
            |> move (-200,-40)

bar w h clr = group [filled clr (rect w h) |> move (0,h/2-140)]

slide3 t =
  let

    rawData : List (String, number, Color)
    rawData = [("2002",20,lightBlue),
               ("2004",40,lightBlue),
               ("2006",80,lightBlue),
               ("2008",160,lightBlue),
               ("2010",320,lightBlue),
               ("2016",1000,darkRed)]

    yLabel = "Insert Unit Here"
    xLabel = "Year"
    title  = "DATA VOLUME"

    label (l,_,_) = l
    number (_,n,_) = n
    colour (_,_,c) = c

    numBars = List.length rawData
    maxim = List.maximum (List.map number rawData)
    rectangles = List.map2 mkRect rawData [0..numBars]

    mkRect (str,n,clr) xPos =
      case maxim of
        Nothing -> rect 5 5   --this case only happens with no data
                     |> filled clr  --just make a rectangle
                     |> move (-100,0)
        Just m  -> bar (width/(toFloat numBars)-10) ((trans ((t - 100 * toFloat xPos)/200) 1)*height*n/m) clr
                     |> move ((toFloat xPos)*(width/(toFloat numBars))-width/3,0 )
                     
    xLabels = List.map2 mkXText rawData [0..numBars]

    mkXText (str,n,clr) xPos = text str
                                 |> customFont "Arial"
                                 |> filled black
                                 |> move ((toFloat xPos)*(width/(toFloat numBars))-width/3
                                            - (width/(toFloat numBars)-10)/4
                                          ,-height/2-15)
    yNums = case maxim of
              Nothing -> []
              Just m  -> List.map ((*)(m//10)) [0..10]

    yLabels = List.map2 mkYText yNums [0..10]

    mkYText num pos = text (toString num)
                    |> customFont "Arial"
                    |> filled black
                    |> move (-width/2-30,pos*height/10-140)

    yText = text yLabel
              |> customFont "Helvetica"
              |> bold
              |> filled black
              |> move (-width/2-60,height/5+10)
              |> rotate (Basics.degrees 90)

    xText = text xLabel
              |> customFont "Helvetica"
              |> bold
              |> filled black
              |> move (0, -height/2-50)

    titleText title = text title
                        |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled red
                        |> move (-150,height/2 + 50)

    width = 300 --total graph width and height
    height = 300
    waves  =  [ group [  wave (t/2) darkBlue
                            |> makeTransparent 0.8,
            wave (t/3) lightBlue
                |> move (-100,0)
                |> makeTransparent 0.5          
              ] |> move(0,-80 + trans (t/4 - 100) 450),
              group [
              text "World's data volume to grow 40%"
                |> customFont "Helvetica"
                |> size 45
                |> bold
                |> filled white


              ,
              text "per year & 50 times by 2020."
                |> customFont "Helvetica"
                |> size 45
                |> bold
                |> filled white
                |> move(0,-50)
                    ] |> move(-450,-160)
                      |> fadeIn t 1000

              ]

  in titleText title::xText::rectangles ++ xLabels ++ yLabels
        ++ waves

wave t clr = curve (0,0) [Pull (100,50*cos(t/50)) (200,0),
                          Pull (300,-50*cos(t/50)) (400,0),
                          Pull (500,50*cos(t/50)) (600,0),
                          Pull (700,-50*cos(t/50)) (800,0),
                          Pull (900,50*cos(t/50)) (1000,0),
                          Pull (1100,-50*cos(t/50)) (1200,0),
                          Pull (1200,-200) (1200,-500),
                          Pull (800, -500) (0,-500)]
            |> filled clr
            |> move (-500,-200)

slide4 t = [ rect 2000 2000
                        |> filled (rgb 230 230 255),
              text "DATA VELOCITY"
                        |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled blue
                        |> move (-170,200)
            , text "Twitter averages at 7000 tweets per second."
                        |> size 25
                        |> customFont "Helvetica"                  
                        |> filled blue
                        |> move (-230,165)
                        |> fadeIn t 1500
            , tweet (rgb 94 169 208)
                  |> move (trans t 1000 - 650,
                           90*sin(trans t 1000/75)-45)
                  |> scaleX (1 - tranSin (t-1000) 2)
                  |> scaleX (1 - tranSin (t-1200) 2)
                  |> move ((trans (t-1400) 1000)*2,40*sin(trans (t-1400) 1000 /10))
            , text "#omg"
                  |> size 50
                  |> customFont "Helvetica"
                  |> bold
                  |> filled darkBlue  
                  |> move (250,110)
                  |> appear t 1150
                  |> disappear t 1210 
            , group (endlessTweets darkBlue t 30 1)
                  |> move (-30,-20)
            , group (endlessTweets blue t 40 2)
                  |> move (-20,-40)
            , group (endlessTweets lightBlue t 45 3)
                  |> move (-10,-60)
            , group (endlessTweets (rgb 94 169 208) t 50 4)
                  |> move (0,-70)
            ]

endlessTweets clr t h x = List.map (createTweet t clr h x) [0..15]

createTweet t clr h x n =  let y = if n % 2 == 0 then h*sin(t/25/x)
                            else h*cos(t/25/x)
                    in
                    tweet clr
                      |> move (-650 + mod (t + toFloat n * 81) 1300,
                               y)
                      |> appear t (2500 - toFloat n*81)


tweet clr = group [curve (0,0) [Pull (75,-25) (85,50),
                            Pull (88, 55) (92,60),
                            Pull (87,58)  (83,56),
                            Pull (86,63) (90,70),
                            Pull (85,67) (80,64),
                            Pull (47,80) (49,46),
                            Pull (28,42) (7,66),
                            Pull (0,53) (10,40),
                            Pull (7,41) (4,42),
                            Pull (6,30) (20,21),
                            Pull (15,20) (10,20),
                            Pull (20,10) (30,10),
                            Pull (20,0) (0,0)]
            |> filled clr
            ]

wing t = curve (49,46) [                       
                           
                            Pull (28,42) (7,66),
                            Pull (0,53) (10,40),
                            Pull (7,41) (4,42),
                            Pull (6,30) (20,21),
                            Pull (15,20) (10,20),
                            Pull (20,10) (30,10)]
            |> filled lightBlue
            
slide5 t = [  rect 1000 500
                        |> filled black
            , text "DATA VARIETY"
                        |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled green
                        |> move (-160,200)
            , wallOfBinary t
            , group [text "What classifies as data?"
                        |> customFont "Helvetica"
                        |> size 20
                        |> bold
                        |> filled (rgb 243 243 21)
                        |> move (-115,170)
                        |> fadeIn t 100
            , fadeText t "Text?" 300
                |> move (-30,120)
            , fadeText t "Numbers?" 400
                |> move (140,80)
            , fadeText t "Images?" 500
                |> move (-360,90)
            , fadeText t "Audio?" 600
                |> move (40,-140)
            , fadeText t "Video?" 700
                |> move (-140,-100)
            , fadeText t "Geolocation?" 800
                |> move (-350,-180)
            , fadeText t "Call history?" 900
                |> move (220,-50)
            , fadeText t "Medical records?" 1000
                |> move (-240,20)
            ] |> fadeOut t 1300
             , text "What doesn't?"  
                  |> customFont "Helvetica"
                  |> size 45
                  |> bold
                  |> filled white
                  |> move (-145,-19)
                  |> fadeIn t 1600
            ]

fadeText t str offSet = text str
                        |> customFont "Helvetica"
                        |> size 24
                        |> bold
                        |> filled white
                        |> fadeIn t offSet

wallOfBinary t = group (List.map (endlessBinary t) [0..10]) |> makeTransparent (0.5)

endlessBinary t y = let x = if y%2 == 0 then 1 else -1
                    in
                    group (List.map2 (binary t x) 
                              [0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,1,
                               0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,1]
                              [0..31]) |> move (0,130 - toFloat y * 40)


binary t  x n offSet=  let n' = n
                    in
                    text (toString n) 
                      |> customFont "Courier New"
                      |> size 25
                      |> bold
                      |> filled green
                      |> move ((-650 + mod (t/4 + toFloat offSet * 40.5) 1300) * x,
                               0)
                


slide6 t = [text "MORE"
                      |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled blue
                        |> move (-70,200),
            person (rgb 245 121 0),
            personCircle (colorChange t 500),
            magnifyingGlass
              |> move (0,-10)
              |> scale (1 + tranSin (t-500) 1.1)
              |> rotate (Basics.degrees (tranSin (t-500) -90))
            ,
            text "Sampling is a relic of the past."
              |> customFont "Helvetica"
              |> size 25
              |> filled blue
              |> move (-155,170)
              |> fadeOut t 500,
            text "Start using"
              |> customFont "Helvetica"
              |> size 25
              |> filled blue
              |> move (220,20)
              |> fadeIn t 600,
            text "ALL OF IT."
              |> customFont "Helvetica"
              |> size 35
              |> bold
              |> underline
              |> filled blue
              |> move (220,-20)
              |> fadeIn t 680
              ]

colorChange t n = rgb (52  + (trans (t-n) 193))
                      (101 + (trans (t-n) 20))
                      (164 - (trans (t-n) 164))

magnifyingGlass = group [ 
                          rect 24 160
                              |> filled (rgb 60 60 60)
                              |> rotate (Basics.degrees 45)
                              |> move (120,-120),
                          circle 90
                              |> filled (rgba 255 255 255 0.2)
                              |> addOutline (solid 12) (rgb 125 125 125)
                         
                          
                  ]

person c = group [group [ filled c (circle 45) |> move (0,130),
                 filled c (rect 100 150),
                 filled c (rect 45 152) |> move (-27.5,-149),
                 filled c (rect 45 152) |> move (27.5,-149),
                 filled c (circle 22.5) |> move (-27.5,-224),
                 filled c (circle 22.5) |> move (27.5,-224),
                 arms c |> move (0,-10)
                 ] |> scale (0.3)]

personCircle c = group [  person c |> move (80,0),
                          person c |> move (-80,0),
                          person c |> move (40,100),
                          person c |> move (-40,100),
                          person c |> move (40,-100),
                          person c |> move (-40,-100)
                      ]


arms c = group [filled c (rect 45 130) |> move (-75,0),
                 filled c (rect 45 130) |> move (75,0),
                 filled c (circle 22.5) |> move (-75,-65),
                 filled c (circle 22.5) |> move (75,-65),
                 filled c (circle 22.5) |> move (-75,65),
                 filled c (circle 22.5) |> move (75,65)]

slide7 t = [text "MESSINESS"
                      |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled blue
                        |> move (-120,200),
            text "Learn to accept errors and imperfections."
              |> customFont "Helvetica"
              |> size 25
              |> filled blue
              |> move (-205,170)
              |> fadeOut t 1200
              ,
            text "Try and see the pattern."
              |> customFont "Helvetica"
              |> size 25
              |> filled blue
              |> move (-117,170)
              |> fadeIn t 1200
              
            ,
            plot
              |> move (30,-30)
            ,
            dots t
              |> move (35,-30)
              |> makeTransparent (1 - tranSin (t-1200) 0.5)
            ,
            line (-120,-90) (-120 + tranSin (t-1200) 270, -90 + tranSin (t-1200) 126)
              |> outlined (dashed 2) blue

              ]

plot =  group [      line (-150,-150) (150,-150)
                            |> outlined (solid 2) blue,
                     ngon 3 6
                            |> filled blue
                            |> move (150,-150),
                     ngon 3 6
                            |> filled blue
                            |> move (-150,150) 
                            |> rotate (Basics.degrees 90),
                     line (-150,-150) (-150,150)
                            |> outlined (solid 2) blue]


dots t = let dotb tOff = filled (rgba 87 87 245 (tranSin (t-tOff) 1)) (circle 6)
             dotr tOff = filled (rgba 245 87 87 (tranSin (t-tOff) 1)) (circle 6)
         in group[dotb 0    |> move (-5,45),
                  dotb 50  |> move (-40,65), 
                  dotb 100  |> move (-70,30), 
                  dotb 150  |> move (-115,50), 
                  dotb 200  |> move (-110,110), 
                  dotb 250  |> move (-58,122), 
                  dotb 300  |> move (-80,80), 
                  dotb 350  |> move (-10,130), 
                  dotb 400  |> move (21,80), 
                  dotb 450  |> move (-15,95), 
                  dotb 500 |> move (30,100), 
                  dotb 550 |> move (-134,78), 
                  dotr 600 |> move (108,-23), 
                  dotr 650 |> move (60,-49), 
                  dotr 700 |> move (80,-76), 
                  dotr 750 |> move (10,-92), 
                  dotr 800 |> move (5,-50), 
                  dotr 850 |> move (-30,-70), 
                  dotr 900 |> move (-50,-120), 
                  dotr 950 |> move (-90,-80), 
                  dotr 1000 |> move (-8,-25), 
                  dotr 1050 |> move (42,-70), 
                  dotr 1100 |> move (63,-8), 
                  dotr 1150 |> move (32,-100)]

slide8 t = [ backdrop t,  
            text "CORRELATION"
                      |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled blue
                        |> move (-140,200),
            text "(noun) A mutual relationship between two or more things."
              |> customFont "Helvetica"
              |> italic
              |> size 25
              |> filled blue
              |> move (-285,170)
            ,
            group [
            text "WHY?"
              |> customFont "Helvetica"
              |> centered
              |> size 50
              |> bold
              |> filled darkGreen
            , cross (t-400)
              |> move (0,10)
            ] |> fadeOut t 600

            ,
            text "WHAT?"
              |> customFont "Helvetica"
              |> size 50
              |> centered
              |> bold
              |> filled red

              |> fadeIn t 700
            ,
            check
              |> move (100,-20)
              |> fadeIn t 900
            ]

cross t = group [line (-50,-50) (-50 + tranSin (t) 100, -50 + tranSin (t) 100)
              |> outlined (solid 16) red,
           line (-50,50) (-50 + tranSin (t-100) 100, 50 - tranSin (t-100) 100)
              |> outlined (solid 16) red]

check  = openPolygon [(-50,-25),(-25,-50),(50,50)]
              |> outlined (solid 16) green

backdrop  t = group[ polygon [
                  (-500,-250),(-500,0),
                    (-250,20),(0,50),
                    (250,95),(500,170),
                    (500,-250)]
              |> filled (rgb 255 105 180)
              |> makeTransparent (0.5)
              ,
              polygon [(-500,-250),
                    (-500,0),
                    (-400,27),
                    (-300,-2),
                    (-200,54),
                    (-100,45),
                    (0,70),
                    (90,85),
                    (200,50),
                    (300,95),
                    (400,115),
                    (500,180),
                    (500,-250)]
              |> filled (rgb 5 245 253)
              |> makeTransparent (0.5)
              , rect 1000 500
              |> filled white
              |> move (trans (t-1000) 1100,0)
              ]
slide9 t = [ rect 1000 500
                        |> filled darkBlue
            , locks t,
             text "DANGERS"
                        |> customFont "Helvetica"
                        |> size 45
                        |> bold
                        |> filled white
                        |> move (-100,200)
            ,
            text "Is your data secure?"
                        |> customFont "Helvetica"
                        |> size 25
                        |> filled white
                        |> move (-99,170)
            ,
             pacman t
                  |> move (-700 + trans (t-800) 2000,0)   
                        ]

locks t = group (List.map (lock t) [0..5])

lock t n = let off = n * 162 in
           group [
              rect 30 250
                |> filled blue
                |> move (-400 + off,125 + tranSin (t-550-off) 125),
              rect 30 250
                |> filled blue
                |> move (-400 + off,-125 - tranSin (t-550-off) 125),
              group [
              curve (0,-30) [Pull (0,25) (0,50),
                           Pull (0,100) (50,100),
                           Pull (100,100) (100,50),
                           Pull (100,40) (100,30),
                           Pull (90,30) (80,30),
                           Pull (80,80) (50,80),
                           Pull (20,80) (20,30),
                           Pull (20,0) (20,-30)]
                    |> filled (rgb 80 80 80)
                    |> move (-50,20 + tranSin (t-400-off) 30),
              roundedRect 120 120 10
                    |> filled (rgb 130 130 130),
              circle 20
                    |> filled (rgb 80 80 80)
                    |> move (0,20),
              triangle 20
                    |> filled (rgb 80 80 80)
                    |> move (0,-20)
                    |> rotate (Basics.degrees 90)
                    |> scaleX (2)
              ] |> vibrate t (200 + off) 10
                |> move (-400 + off,-20 - 0.05 * (trans (t-550-off) 1000)^2)
              ]

slide10 t = [ 

              barchart t |> move (-490,-120)
                    , 


              text "BIG DATA"
                   |> customFont "Helvetica"
                   |> size 205
                   |> bold
                   |> filled white
                   |> move (-480,-220)     
              ,
              text "Here on out, it only gets bigger."
                   |> customFont "Helvetica"
                   |> size 15
                   |> bold
                   |> filled blue
                   |> move (-100,200)  
            ]

barchart t = let bar' off = bar 20 (200 + off ^ 2 /8 + 30*sin(t/100-off)) blue |> move (20*off,0)
                 bars = List.map bar' [0..49]
             in
             group bars