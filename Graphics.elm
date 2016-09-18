module Graphics exposing(..)

import Html
import Html.App
import Html.Events
import Svg
import Svg.Attributes exposing (..)
import String exposing (..)
import Svg exposing (Attribute)
import Array
import Mouse exposing (Position)
import Json.Decode as Json exposing (..)

type Stencil = Circle Float
             | Rect Float Float
             | RoundRect Float Float Float
             | Oval Float Float
             | BezierPath (Float,Float) (List(((Float,Float),(Float,Float))))
             | Polygon (List (Float,Float))
             | Path (List (Float,Float))
             | Text Face String

type Shape notification = Inked Color (Maybe (LineType, Color)) Stencil
           | Move (Float,Float) (Shape notification)
           | Rotate Float (Shape notification)
           | ScaleXY Float Float (Shape notification)
           | Group (List (Shape notification))
           | Tap notification (Shape notification)
           | TapAt ((Float,Float) -> notification) (Shape notification)
           | Enter notification (Shape notification)
           | EnterAt ((Float,Float) -> notification) (Shape notification)
           | Exit notification (Shape notification)
           | ExitAt ((Float,Float) -> notification) (Shape notification)

type Color = RGBA Float Float Float Float

type LineType = Solid Float
              | Broken (List (Float,Float)) Float -- length of lines and gaps in pixels

type Face = Face Float -- size
                 Bool -- bold
                 Bool -- italic
                 Bool -- underline
                 Bool -- strikethrough
                 Bool -- selectable
                 Font

type Font = Serif | Sansserif | FixedWidth | Custom String

type Pull = Pull (Float,Float) (Float,Float)

       {-    | OnMouseDown 
           | OnMouseUp
           
           | OnDoubleClick
           | OnInput -}

line : (Float,Float) -> (Float,Float) -> Stencil
line p1 p2 = Path [p1,p2]

polygon : List (Float,Float) -> Stencil
polygon ptList = Polygon ptList

openPolygon: List(Float,Float) -> Stencil
openPolygon ptList = Path ptList

ngon n r = Polygon <| List.map (ptOnCircle r n) [0..n]

triangle : Float -> Stencil
triangle r = ngon 3 r

square r = Rect r r

rect w h = Rect w h

roundedRect w h r = RoundRect w h r

rectangle w h = Rect w h

circle r = Circle r

oval x y = Oval x y

graphPaper s = group (List.map (createGraphX 1600 s) [-1500/s..1500/s] ++ List.map (createGraphY 3000 s) [-800/s..800/s])
createGraphX h s x = filled (rgb 135 206 250) (rect 1 h) |> move(x*s,0)
createGraphY w s y = filled (rgb 135 206 250) (rect w 1) |> move(0,y*s)

funnyStar r n frac = Polygon <| [(0,0)]
                       ++ (List.map ((ptOnCircle r n) << ((*)(frac/10*180))) [-10..10])
                       ++[(0,0)]

wedge r frac =  let n = frac*360/10 + 5
                in
                Polygon <| [(0,0)]
                       ++ (List.map ((wedgeHelper r) << ((*)(frac/n*180))) [-n..n])
                       ++[(0,0)]

wedgeHelper r cn = let angle = cn
                    in
                    (r * cos (degrees angle), r * sin (degrees angle))

ptOnCircle r n cn = let angle = 360 * cn / n
                    in
                    (r * cos (degrees angle), r * sin (degrees angle))

curve: (Float,Float) -> List Pull -> Stencil
curve (a,b) list = BezierPath (a,b) (List.map curveListHelper list)

curveListHelper (Pull (a,b) (c,d)) = ((a,b),(c,d))

text: String -> Stencil
text str = Text (Face 12 False False False False False Serif) str

curveHelper: Shape notification -> Shape notification
curveHelper shape = case shape of 
                        Inked clr outline (BezierPath (a,b) list) -> group [shape, generateCurveHelper (a,b) list ]
                        Move s shape -> Move s (curveHelper shape)
                        Rotate r shape -> Rotate r (curveHelper shape)
                        ScaleXY sx sy shape -> ScaleXY sx sy (curveHelper shape)
                        Group list -> Group (List.map curveHelper list)
                        a -> a

generateCurveHelper (a,b) list = let l1Array = Array.fromList ([(a,b)] ++ List.concat (List.map createTopLevelList list))
                                 in group [generateCHLines l1Array, generateCHCircles l1Array]

generateCHLines ar = let len = Array.length ar
                           in group (List.map (generateCHLine ar) [0..(len-2)])

generateCHLine ar int = let p1 = case (Array.get int ar) of
                                    Just p -> p
                                    Nothing -> (0,0)
                            p2 = case (Array.get (int+1) ar) of
                                    Just p -> p
                                    Nothing -> (0,0)
                        in
                    outlined (dashed 0.5) black (line (p1) (p2))

generateCHCircles ar = let len = Array.length ar
                            in group (List.map (generateCHCircle ar) [0..(len-1)])

generateCHCircle ar int = let p1 = case (Array.get int ar) of
                                    Just p -> p
                                    Nothing -> (0,0)
                              ptStr = pairToString p1
                          in group [filled red (circle 2), text ("("++ptStr++")") |> filled black |> move(5,5) ] |> move p1

createTopLevelList ((a,b),(c,d)) = [(a,b),(c,d)]

--group: (List Shape) ->

type alias Transform = (((Float,Float)  -- normal transformation of whole group
                        ,(Float,Float)
                        ,(Float,Float)
                        )
                        ,((Float,Float),Float,(Float,Float)) -- scale/rotate/shift inside groups
                        )

coalesce (((a,b),(c,d),(tx,ty)),((sx,sy),rot,(shx,shy)))
  = let sa = sx*a
        sb = sy*b
        sc = sx*c
        sd = sy*d
        rx = cos rot
        ry = sin rot
    in (((rx * sa - ry * sb, ry * sa + rx * sb)
        ,(rx * sc - ry * sd, ry * sc + rx * sd)
        ,(tx + a*shx + c*shy, ty + b*shx + d*shy))
       ,((1,1),0,(0,0))
       )

id = (((1,0)
      ,(0,1)
      ,(0,0)
      )
     ,((1,1),0,(0,0))
     )

moveT : Transform -> (Float,Float) -> Transform
moveT (trans,(s,r,(tx,ty))) (u,v) = (trans,(s,r,(tx+u,ty+v)))
rotT (trans,(s,r,t)) rad = (trans,(s,r+rad,t))
scaleT (trans,((ssx,ssy),r,(shx,shy))) (sx,sy) = (trans,((ssx*sx,ssy*sy),r,(shx,shy)))

--collage : Float -> Float -> (List Shape) -> Html.Html msg
collage w h shapes = Svg.svg
                     [ width "100%", height "100%", viewBox ((toString (-w/2)) ++ " " ++ (toString (-h/2)) ++ " " ++ (toString w) ++ " " ++ (toString h))]
                     (List.map (createSVG id) shapes)

f = 500 --focal length
--puppetShow : Float -> Float -> List (Float,Shape) -> Html.Html msg
puppetShow w h listShapes = collage w h (List.map extractShape (List.sortWith flippedComparison listShapes))
--extractShape: (Float,Shape notification) -> Shape notification
extractShape (z,shape) = let s = f/(f+z) 
                             in group [shape] |> scale s

flippedComparison (a,x) (b,y) =
   case compare a b of
     LT -> GT
     EQ -> EQ
     GT -> LT

--Notification functions

notifyTap msg shape = Tap msg shape
notifyEnter msg shape = Enter msg shape
notifyLeave msg shape = Exit msg shape
notifyTapAt msg shape = TapAt msg shape
notifyEnterAt msg shape = EnterAt msg shape
notifyLeaveAt msg shape = ExitAt msg shape

xyToPair xy = (Basics.toFloat xy.x,Basics.toFloat xy.y)

--onTapAt : Attribute Msg
onTapAt msg =
  Html.Events.on "click" 
            (Json.map (msg << xyToPair) Mouse.position)
                
onEnterAt msg =
  Html.Events.on "mouseover" 
            (Json.map (msg << xyToPair) Mouse.position)

onLeaveAt msg =
  Html.Events.on "mouseleave" 
            (Json.map (msg << xyToPair) Mouse.position)

--createSVG : Transform -> Shape -> Svg.Svg msg
createSVG trans shape =
    case shape of
      Inked fillClr lt stencil
        -> let (((a,b),(c,d),(tx,ty)),_) = coalesce trans
               attrs = transAttrs ++ clrAttrs ++ strokeAttrs
               transAttrs = [Svg.Attributes.transform <| "matrix("++(String.concat <| List.intersperse "," <| List.map toString [a,-b,c,-d,tx,-ty])++")"]
               clrAttrs = [ fill (mkRGB fillClr), fillOpacity (mkAlpha fillClr)]
               strokeAttrs = case lt of
                               Nothing -> []
                               Just (Solid w , strokeClr) -> [ strokeWidth (toString w)
                                                           , stroke (mkRGB strokeClr), strokeOpacity (mkAlpha strokeClr)]
                               Just (Broken dashes w , strokeClr) -> [ strokeWidth (toString w)
                                                                   , stroke (mkRGB strokeClr), strokeOpacity (mkAlpha strokeClr)]
                                                                   ++ [strokeDasharray <| String.concat (List.intersperse "," <| List.map pairToString dashes)]
           in (case stencil of
                  Circle r -> Svg.circle ([ cx "0", cy "0"
                                            , Svg.Attributes.r (toString r) ] ++ attrs) []
                  Rect w h -> Svg.rect ([ x (toString (-w/2)), y (toString (-h/2))
                                          , width (toString w), height (toString h)] ++ attrs) []
                  RoundRect w h r -> Svg.rect ([ x (toString (-w/2)), y (toString (-h/2))
                                                 , rx (toString r), ry (toString r)
                                                 , width (toString w), height (toString h)] ++ attrs) []
                  Oval w h -> Svg.ellipse ([ cx "0", cy "0"
                                             , rx (toString (0.5*w)), ry (toString (0.5*h)) ] ++ attrs) []
                  -- BezierPath (List )
                  Polygon vertices -> Svg.polygon ([points <| String.concat <| List.intersperse " " <| List.map pairToString vertices]
                                                   ++ attrs) []
                  Path vertices -> Svg.polyline ([points <| String.concat <| List.intersperse " " <| List.map pairToString vertices]
                                                   ++ attrs) []
                  BezierPath start pts -> Svg.path ([Svg.Attributes.d <| (createBezierString start pts)]
                                                                     ++ attrs) []
                  Text (Face si bo i u s sel f) str -> let  bol = if bo then "font-weight: bold;" else ""
                                                            it = if i then "font-style: italic;" else ""
                                                            un = if u then "text-decoration: underline;" else ""
                                                            stri = if s then "text-decoration: strikethrough;" else ""
                                                            select = if not sel then "-webkit-touch-callout: none;
                                                                                  -webkit-user-select: none;
                                                                                  -khtml-user-select: none;
                                                                                  -moz-user-select: none;
                                                                                  -ms-user-select: none;
                                                                                  user-select: none;"
                                                                      else ""

                                                            font = case f of 
                                                                 Sansserif -> "sansserif;" 
                                                                 FixedWidth -> "fixedwidth;"
                                                                 Custom fStr -> fStr ++ ";"
                                                                 _ -> "serif"
                                                            sty = bol ++ it ++ un ++ stri ++
                                                                "font-family: " ++ font ++ select
                                                        in Svg.text' ([ x (toString 0), y (toString 0), Svg.Attributes.style sty, Svg.Attributes.fontSize (toString si)] ++ attrs ++ [Svg.Attributes.transform <| "matrix("++(String.concat <| List.intersperse "," <| List.map toString [a,b,c,d,tx,-ty])++")"]) [Svg.text str]
                )
      Move v shape                           -> createSVG (moveT trans v) shape
      Rotate deg shape                       -> createSVG (rotT trans deg) shape
      ScaleXY sx sy shape                    -> createSVG (scaleT trans (sx,sy)) shape
      Tap msg shape                          -> Svg.g [Html.Events.onClick msg] [createSVG (coalesce trans) shape]
      TapAt msg shape                        -> Svg.g [onTapAt msg] [createSVG (coalesce trans) shape]
      Enter msg shape                        -> Svg.g [Html.Events.onMouseEnter msg] [createSVG (coalesce trans) shape]
      EnterAt msg shape                      -> Svg.g [onEnterAt msg] [createSVG (coalesce trans) shape]
      Exit msg shape                         -> Svg.g [Html.Events.onMouseLeave msg] [createSVG (coalesce trans) shape]
      ExitAt msg shape                       -> Svg.g [onLeaveAt msg] [createSVG (coalesce trans) shape]
      Group shapes -> Svg.g [] <| List.map (createSVG <| coalesce trans) shapes

--Filling / outlining functions
filled: Color -> Stencil -> Shape notification
filled color shape = Inked color Nothing shape

outlined: LineType -> Color -> Stencil -> Shape notification
outlined style outlineClr shape = let lineStyle = (style, outlineClr)
                                  in Inked (rgba 0 0 0 0) (Just lineStyle) shape

addOutline: LineType -> Color -> Shape notification -> Shape notification
addOutline style outlineClr shape = let lineStyle = (style, outlineClr)
                                  in
                                  case shape of
                                    Inked clr outline shape -> Inked clr (Just lineStyle) shape
                                    Move s shape -> Move s (addOutline style outlineClr shape)
                                    Rotate r shape -> Rotate r (addOutline style outlineClr shape)
                                    ScaleXY sx sy shape -> ScaleXY sx sy (addOutline style outlineClr shape)
                                    Group list -> Group list
                                    a -> a

makeTransparent: Float -> Shape notification -> Shape notification
makeTransparent alpha shape = case shape of
                                    Inked (RGBA r g b a) (Just (lineType, (RGBA sr sg sb sa))) shape -> Inked (RGBA r g b (a*alpha)) (Just (lineType, (RGBA sr sg sb (sa*alpha)))) shape
                                    Inked (RGBA r g b a) Nothing shape -> Inked (RGBA r g b (a*alpha)) Nothing shape
                                    Move s shape -> Move s (makeTransparent alpha shape)
                                    Rotate r shape -> Rotate r (makeTransparent alpha shape)
                                    ScaleXY sx sy shape -> ScaleXY sx sy (makeTransparent alpha shape)
                                    Group list -> Group (List.map (makeTransparent alpha) list)
                                    a -> a

--Line styles
solid th = Solid th
dotted th = Broken [(th,th)] th
dashed th = Broken [(th*5,th*2.5)] th
longdash th = Broken [(th*12,th*6)] th
dotdash th = Broken [(th,th),(th*5,th)] th
custom list th = Broken list th
increasing s e th = Broken (List.map makePair [s..e]) th
makePair n = (n,n)

--Text functions
size size stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face size bo i u s sel f) str
                  a -> a
bold stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si True i u s sel f) str
                  a -> a
italic stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si bo True u s sel f) str
                  a -> a
underline stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si bo i True s sel f) str
                  a -> a
strikethrough stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si bo i u True sel f) str
                  a -> a

selectable stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si bo i u s True f) str
                  a -> a

sansserif stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si bo i u s sel Sansserif) str
                  a -> a

customFont fStr stencil = case stencil of 
                  (Text (Face si bo i u s sel f) str) -> Text (Face si bo i u s sel (Custom fStr)) str
                  a -> a

--Transformation functions
rotate theta shape = Rotate theta shape
move disp shape = Move disp shape
scale s shape = ScaleXY s s shape
scaleX s shape = ScaleXY s 1 shape
scaleY s shape = ScaleXY 1 s shape
mirrorX shape = ScaleXY -1 1 shape
mirrorY shape = ScaleXY 1 -1 shape
group shapes = Group shapes
rgb r g b = RGBA r g b 1
rgba r g b a = RGBA r g b a
pairToString (x,y) = (toString x)++","++(toString y)

createBezierString first list = "M " ++ (pairToString first) ++ String.concat (List.map bezierStringHelper list)
bezierStringHelper ((a,b),(c,d)) = " Q " ++ pairToString (a,b) ++ " " ++ pairToString (c,d)

mkAlpha (RGBA _ _ _ a) = toString a
mkRGB (RGBA r g b _) = "#" ++ (toHex <| round r) ++ (toHex <| round g) ++ (toHex <| round b)

toHex: Int -> String
toHex dec = let first = dec // 16
                second = (dec % 16)
            in (toHexHelper first) ++ (toHexHelper second)

toHexHelper: Int -> String
toHexHelper dec = case dec of
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "A"
    11 -> "B"
    12 -> "C"
    13 -> "D"
    14 -> "E"
    15 -> "F"
    _ -> ""


--
pink: Color
pink =
    RGBA 255 105 180 1

hotPink: Color
hotPink =
    RGBA 255 0 66 1

{-|-}
lightRed : Color
lightRed =
  RGBA 239 41 41 1


{-|-}
red : Color
red =
  RGBA 204 0 0 1


{-|-}
darkRed : Color
darkRed =
  RGBA 164 0 0 1


{-|-}
lightOrange : Color
lightOrange =
  RGBA 252 175 62 1


{-|-}
orange : Color
orange =
  RGBA 245 121 0 1


{-|-}
darkOrange : Color
darkOrange =
  RGBA 206 92 0 1


{-|-}
lightYellow : Color
lightYellow =
  RGBA 255 233 79 1


{-|-}
yellow : Color
yellow =
  RGBA 237 212 0 1


{-|-}
darkYellow : Color
darkYellow =
  RGBA 196 160 0 1


{-|-}
lightGreen : Color
lightGreen =
  RGBA 138 226 52 1


{-|-}
green : Color
green =
  RGBA 115 210 22 1


{-|-}
darkGreen : Color
darkGreen =
  RGBA 78 154 6 1


{-|-}
lightBlue : Color
lightBlue =
  RGBA 114 159 207 1


{-|-}
blue : Color
blue =
  RGBA 52 101 164 1


{-|-}
darkBlue : Color
darkBlue =
  RGBA 32 74 135 1


{-|-}
lightPurple : Color
lightPurple =
  RGBA 173 127 168 1


{-|-}
purple : Color
purple =
  RGBA 117 80 123 1


{-|-}
darkPurple : Color
darkPurple =
  RGBA 92 53 102 1


{-|-}
lightBrown : Color
lightBrown =
  RGBA 233 185 110 1


{-|-}
brown : Color
brown =
  RGBA 193 125 17 1


{-|-}
darkBrown : Color
darkBrown =
  RGBA 143 89 2 1


{-|-}
black : Color
black =
  RGBA 0 0 0 1


{-|-}
white : Color
white =
  RGBA 255 255 255 1


{-|-}
lightGrey : Color
lightGrey =
  RGBA 238 238 236 1


{-|-}
grey : Color
grey =
  RGBA 211 215 207 1


{-|-}
darkGrey : Color
darkGrey =
  RGBA 186 189 182 1


{-|-}
lightGray : Color
lightGray =
  RGBA 238 238 236 1


{-|-}
gray : Color
gray =
  RGBA 211 215 207 1


{-|-}
darkGray : Color
darkGray =
  RGBA 186 189 182 1


{-|-}
lightCharcoal : Color
lightCharcoal =
  RGBA 136 138 133 1


{-|-}
charcoal : Color
charcoal =
  RGBA 85 87 83 1


{-|-}
darkCharcoal : Color
darkCharcoal =
  RGBA 46 52 54 1



