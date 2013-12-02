module Main where

--import Graphics.Collage
import Graphics.Input
import FormFactor
import Util (labelledField,labelledChoice,range,labove,getFloat)
import Window
import Collapse
import Fields (updater,fieldMaker)
import Graph (makePoints,Axis,Linear,Log,axisMaker,canvas,ticMaker)


qGroup = Graphics.Input.fields ("Name","Value")
qRange rec = range rec.qmin rec.qmax (rec.qcount+1)

qCondenser (name,val) rec = case name of
                            "qmin" -> {rec - qmin | qmin = getFloat val}
                            "qmax" -> {rec - qmax | qmax = getFloat val}
                            "qcount" -> {rec - qcount | qcount = getFloat val}
                            _ -> rec
                            
qLabelWidth = widthOf . plainText . show <| "qcount"
qSignal = updater qCondenser {qmin=0.001,qmax=20,qcount=1000} qGroup.events
qTitle = text . Text.color blue . toText <|  "Q-Range"
qFields = flow down <| [fieldMaker qGroup qLabelWidth "qmin" "0.001",
                        fieldMaker qGroup qLabelWidth "qmax" "20",
                        fieldMaker qGroup qLabelWidth "qcount" "1000"]

(qButton,qCollapse) = Collapse.collapsibleSignal qTitle qFields
qBox = Collapse.collapsible qButton qFields


xaxisKind = labelledChoice "X-axis" [("Linear",Linear),("Log",Log)]
yaxisKind = labelledChoice "Y-axis" [("Linear",Linear),("Log",Log)]


xaxis : Signal (Float->Float)
xaxis = lift4 axisMaker (snd xaxisKind) (lift toFloat width) (lift (\x -> x.qmin) qSignal) (lift (\x -> x.qmax) qSignal)
yaxis = lift4 axisMaker (snd yaxisKind) (lift toFloat height) (lift (foldr1 min . map snd)  plotPoints) (lift (foldr1 max . map snd) plotPoints)

xtics = lift2 (\kind pts -> ticMaker kind <| map snd pts) (snd xaxisKind) plotPoints

ytics = lift2 (\kind pts -> ticMaker kind <| map snd pts) (snd yaxisKind) plotPoints
--qmin = labelledField "Q-min" "0.0"
--qmax = labelledField "Q-max" "100.0"
--qcount = labelledField "Q-samples" "100"


--Axis stuff



plotPoints : Signal [(Float,Float)]
plotPoints = lift2 makePoints (lift qRange qSignal) (lift FormFactor.hardSphere FormFactor.hsSignal)

scene terms = flow right <| terms

graphCanvas : Signal Element
graphCanvas = lift7 canvas width height xaxis (snd xaxisKind) yaxis (snd yaxisKind) plotPoints

--testing data
--testread : (Float->Float) -> (Float->Float) -> [(Float,Float)] -> Element
--testread xax yax = +textHeightplainText . show . head . (projectPoints xax yax)


--sizeBox = foldl (lift2 above) (fst qcount) (map fst [qmax, qmin])

width= lift (\x -> x - 300) Window.width
height= Window.height


sidebar : Signal Element
sidebar = lift FormFactor.hsBox FormFactor.hsCollapse `labove` lift qBox qCollapse `labove` fst xaxisKind `labove` fst yaxisKind


main = lift scene <| combine [graphCanvas , sidebar]
