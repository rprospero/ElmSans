module Graph where

import Graphics.Collage

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
canvas w h xax yax points = Graphics.Collage.collage w h [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points)]

--base : [Float]
--base = range 0 100 1

makePoints : [Float] -> (Float -> Float) -> [(Float,Float)]
makePoints base f = zip base <| map f base
