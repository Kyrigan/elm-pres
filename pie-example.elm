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
             in collage 1000 500 [ 
             text "#JustMillennialThings"
                |> size 40
                |> customFont "Helvetica"
                |> centered
                |> outlined (solid 2) pink
                |> move (0,150)
             ,
             piechart t [("Suh dude - 11%",0.11,optRed),
                         ("Hashtags - 12 %",0.12,optBlue),
                         ("Fake 90's Nostalgia - 6%",0.06,optGreen),
                         ("Fam - 31%",0.31,optPurple),
                         ("It's lit - 38%",0.38,optOrange),
                         ("Other - 2%",0.02,optGold)] 
                         0        -- Rotation of the Pie Chart
                         (180,65) -- Starting Point for Legend
                 |> move (-40,0)
               ]

optRed = rgb 211 94 95
optBlue = rgb 114 147 203
optGreen = rgb 132 186 91
optOrange = rgb 225 151 76
optPurple = rgb 144 103 167
optGold = rgb 204 194 16
optGray = rgb 128 133 133

piechart : Float -> List (String, Float, Color) -> Float -> (Float, Float) -> Shape a 
piechart t data start (x,y) = group ((createPie t data start) ++ (createLegend t data start (x,y)))

createPie : Float -> List (String, Float, Color) -> Float -> List (Shape a)
createPie t data start = case data of 
              (_,x,c) :: xs -> (wedge 100 x 
                                |> filled c
                                |> fadeIn t 100
                                |> rotate (degrees (start + 180 * x))
                                ) :: createPie (t - 100) xs  (start + 360 * x)
              _ -> []

createLegend : Float -> List (String, Float, Color) -> Float -> (Float, Float) -> List (Shape a)
createLegend t data idx (x,y) =  case data of
               (label,_,c) :: xs -> (group [ square 8
                                    |> filled c
                                    |> move (-15,4)
                                     ,
                                  text label
                                    |> size 12
                                    |> customFont "Helvetica"
                                    |> filled c] 
                                        |> move (x,y - 25 * idx)
                                        |> fadeIn t 100) :: createLegend (t - 100) xs (idx + 1) (x,y)
               _ -> []                              


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

