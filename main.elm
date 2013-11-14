module Main where

import Graphics.Input
import Graphics.Collage
import String
import FormFactor


--Helper Function to read a Float
getFloat = fromJust 0 . String.toFloat

liftFloat : Signal String -> Signal Float
liftFloat = lift getFloat

labelit title elem = flow right [plainText title, elem]

labelledField : String -> String-> (Signal Element, Signal Float)
labelledField title dflt = let (elem,str) = Graphics.Input.field dflt
  in (lift (labelit title) elem,liftFloat str)


(radField,radius) = Graphics.Input.field "Radius"


fromJust dflt val = maybe dflt id val


fst (a,_) = a


rad = labelledField "Radius" "0.2"
concentration = labelledField "Concentration" "0.0"
qmin = labelledField "Q-min" "0.0"
qmax = labelledField "Q-max" "100.0"
imin = labelledField "I-min" "0.0"
imax = labelledField "I-max" "0.000001"


--Axis stuff
axisMaker size low high value = (value-low)/(high-low)*size-size/2

xaxis : Signal (Float->Float)
xaxis = lift2 (axisMaker 200) (snd qmin) (snd qmax)
yaxis = lift2 (axisMaker 200) (snd imin) (snd imax)

projectPoints : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> [(Float,Float)]
projectPoints fx fy ps = zip (map (fx . fst) ps) (map (fy . snd) ps)

canvas : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
canvas xax yax points = Graphics.Collage.collage 200 200 [traced (solid lightBlue) <| Graphics.Collage.path (projectPoints xax yax points)]

base : [Float]
base = [0..200]

square x y z = (z-x)*(z-x)+y

makePoints : (Float -> Float) -> [(Float,Float)]
makePoints f = zip base <| map f base

plotPoints : Signal [(Float,Float)]
plotPoints = lift makePoints (lift2 (FormFactor.hardSphere 1.0 (10^(-6))) (snd rad) (snd concentration))

scene terms = flow down <| terms

graphCanvas : Signal Element
graphCanvas = lift3 canvas xaxis yaxis plotPoints

--testing data
testread : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
testread xax yax = plainText . show . head . (projectPoints xax yax)


main = lift scene <| combine [graphCanvas , fst rad,
                              fst concentration,
                              fst qmin, fst qmax,
                              fst imin, fst imax,
                              lift3 testread xaxis yaxis plotPoints]
--main = lift scene <| combine [fst rad, fst concentration, radcon]
