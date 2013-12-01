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

ticMaker : Axis -> [Float] -> [Float]
ticMaker kind =
  case kind of
    Linear -> smartTics 10
    Log -> map (\x -> e^x) . smartTics 10 . map log

--yaxisKind = Graphics.Input.dropDown [("Linear",Linear),("Log",Log)]


projectPoints : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> [(Float,Float)]
projectPoints fx fy ps = zip (map (fx . fst) ps) (map (fy . snd) ps)

canvas : Int -> Int -> (Float->Float) -> Axis -> (Float->Float) -> Axis -> [(Float,Float)] -> Element
canvas w h xax xKind yax yKind points = Graphics.Collage.collage w h [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points), xGrid h xax . ticMaker xKind <| map fst points, yGrid w yax . ticMaker yKind <| map snd points]

--base : [Float]
--base = range 0 100 1

smartTics count pts = let low = minimum pts
                          high = maximum pts
                      in range low high count

xGrid h xax pts = group <| map (traced (solid darkGrey) . Graphics.Collage.path . projectPoints xax id . \x -> [(x,(toFloat h)/(-2)),(x,(toFloat h)/2)]) <| pts


yGrid h yax pts = group <| map (traced (solid darkGrey) . Graphics.Collage.path . projectPoints id yax . \y -> [((toFloat h)/(-2),y),((toFloat h)/2,y)]) <| pts


makePoints : [Float] -> (Float -> Float) -> [(Float,Float)]
makePoints base f = zip base <| map f base
