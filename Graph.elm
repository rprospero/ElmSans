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
    Linear -> smartTics 3
    Log -> map (\x -> e^x) . smartTics 4 . map log

projectPoints : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> [(Float,Float)]
projectPoints fx fy ps = zip (map (fx . fst) ps) (map (fy . snd) ps)

canvas : Int -> Int -> (Float->Float) -> Axis -> (Float->Float) -> Axis -> [(Float,Float)] -> Element
canvas w h xax xKind yax yKind points = Graphics.Collage.collage w h [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points), xGrid h xax . ticMaker xKind <| map fst points, yGrid w yax . ticMaker yKind <| map snd points]

smartTics count pts = let low = minimum pts
                          high = maximum pts
                      in range low high count

xGrid h xax pts = let lines = map (traced (solid darkGrey) . Graphics.Collage.path . projectPoints xax id . \x -> [(x,(toFloat h)/(-2)),(x,(toFloat h)/2)]) <| pts
                      labels = map (\x -> moveY ((toFloat h)/(-2) + 10) . moveX (xax x) <| toForm . plainText . show <| x) <| pts
                  in group [group lines, group labels]


yGrid h xax pts = let lines = map (traced (solid darkGrey) . Graphics.Collage.path . projectPoints id xax . \x -> [((toFloat h)/(-2),x),((toFloat h)/2,x)]) <| pts
                      labels = map (\x -> moveX ((toFloat h)/(-2) + 80) . moveY (xax x) <| toForm . plainText . show <| x) <| pts
                  in group [group lines, group labels]



makePoints : [Float] -> (Float -> Float) -> [(Float,Float)]
makePoints base f = zip base <| map f base
