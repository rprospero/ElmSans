module Graph where

import Graphics.Collage
import Util (range)

data Axis = Linear | Log

log = logBase 10

axisMaker kind size low high value = 
          case kind of
               Linear -> (value-low)/(high-low)*size-size/2
               Log ->  let v = log value
                           l = log low
                           h = log high
                       in (v-l)/(h-l)*size-size/2

--yaxisKind = Graphics.Input.dropDown [("Linear",Linear),("Log",Log)]


projectPoints : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> [(Float,Float)]
projectPoints fx fy ps = zip (map (fx . fst) ps) (map (fy . snd) ps)

canvas : Int -> Int -> (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
canvas w h xax yax points = Graphics.Collage.collage w h [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points), xGrid h xax <| map fst points, yGrid w yax <| map snd points]

--base : [Float]
--base = range 0 100 1

smartTics count pts = let low = minimum pts
                          high = maximum pts
                      in range low high count

xGrid h xax pts = group <| map (traced (solid darkGrey) . Graphics.Collage.path . projectPoints xax id . \x -> [(x,(toFloat h)/(-2)),(x,(toFloat h)/2)]) <| smartTics 5 pts


yGrid h yax pts = group <| map (traced (solid darkGrey) . Graphics.Collage.path . projectPoints id yax . \y -> [((toFloat h)/(-2),y),((toFloat h)/2,y)]) <| smartTics 5 pts


makePoints : [Float] -> (Float -> Float) -> [(Float,Float)]
makePoints base f = zip base <| map f base
