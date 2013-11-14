module Main where

import Graphics.Collage
import FormFactor
import Util (labelledField)
import Window


qmin = labelledField "Q-min" "0.0"
qmax = labelledField "Q-max" "100.0"
imin = labelledField "I-min" "0.0"
imax = labelledField "I-max" "0.000001"

width=lift (\x -> (x*3) `div` 4) Window.width
height=lift (\x -> (x*4) `div` 5) Window.height

--Axis stuff
axisMaker size low high value = (value-low)/(high-low)*size-size/2

xaxis : Signal (Float->Float)
xaxis = lift3 axisMaker (lift toFloat width) (snd qmin) (snd qmax)
yaxis = lift3 axisMaker (lift toFloat height) (snd imin) (snd imax)

projectPoints : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> [(Float,Float)]
projectPoints fx fy ps = zip (map (fx . fst) ps) (map (fy . snd) ps)

canvas : Int -> Int -> (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
canvas w h xax yax points = Graphics.Collage.collage w h [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points)]

base : [Float]
base = [0..100]

makePoints : (Float -> Float) -> [(Float,Float)]
makePoints f = zip base <| map f base

plotPoints : Signal [(Float,Float)]
plotPoints = lift makePoints (lift FormFactor.hardSphere FormFactor.hardParams)

scene terms = flow right <| terms

graphCanvas : Signal Element
graphCanvas = lift5 canvas width height xaxis yaxis plotPoints

--testing data
testread : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
testread xax yax = plainText . show . head . (projectPoints xax yax)


sizeBox = foldl (lift2 above) (fst imax) (map fst [imin, qmax, qmin])

main = lift scene <| combine [graphCanvas , lift2 above FormFactor.hardBox 
                              sizeBox,
                              lift3 testread xaxis yaxis plotPoints,
                              lift (plainText . show) height]

