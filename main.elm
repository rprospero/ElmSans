module Main where

import Graphics.Collage
import Graphics.Input
import FormFactor
import Util (labelledField,labelledChoice,range,labove)
import Window


qmin = labelledField "Q-min" "0.0"
qmax = labelledField "Q-max" "100.0"
qcount = labelledField "Q-samples" "100"
imin = labelledField "I-min" "0.0"
imax = labelledField "I-max" "0.000001"

width=lift (\x -> (x*3) `div` 4) Window.width
height=lift (\x -> (x*4) `div` 5) Window.height

--Axis stuff

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
xaxisKind = labelledChoice "X-axis" [("Linear",Linear),("Log",Log)]
yaxisKind = labelledChoice "Y-axis" [("Linear",Linear),("Log",Log)]

xaxis : Signal (Float->Float)
xaxis = lift4 axisMaker (snd xaxisKind) (lift toFloat width) (snd qmin) (snd qmax)
yaxis = lift4 axisMaker (snd yaxisKind) (lift toFloat height) (snd imin) (snd imax)

projectPoints : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> [(Float,Float)]
projectPoints fx fy ps = zip (map (fx . fst) ps) (map (fy . snd) ps)

canvas : Int -> Int -> (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
canvas w h xax yax points = Graphics.Collage.collage w h [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points)]

--base : [Float]
--base = range 0 100 1

makePoints : [Float] -> (Float -> Float) -> [(Float,Float)]
makePoints base f = zip base <| map f base

plotPoints : Signal [(Float,Float)]
plotPoints = lift2 makePoints (lift3 range (snd qmin) (snd qmax) (snd qcount)) (lift FormFactor.hardSphere FormFactor.hardParams)

scene terms = flow right <| terms

graphCanvas : Signal Element
graphCanvas = lift5 canvas width height xaxis yaxis plotPoints

--testing data
testread : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
testread xax yax = plainText . show . head . (projectPoints xax yax)


sizeBox = foldl (lift2 above) (fst imax) (map fst [imin, qcount, qmax, qmin])

main = lift scene <| combine [graphCanvas , FormFactor.hardBox `labove` 
                              sizeBox `labove` fst xaxisKind `labove` 
                              fst yaxisKind,
                              lift3 testread xaxis yaxis plotPoints,
                              lift (plainText . show) height]

