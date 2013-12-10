module Main where

--import Graphics.Collage
import Graphics.Input
import Graphics.Element
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
qTitle = text . Text.color (Color 0 0 255 1) . toText <|  "▼ Q-Range"
qFields = flow down <| [fieldMaker qGroup qLabelWidth "qmin" "0.001",
                        fieldMaker qGroup qLabelWidth "qmax" "20",
                        fieldMaker qGroup qLabelWidth "qcount" "1000"]

(qButton,qCollapse) = Collapse.collapsibleSignal qTitle qFields
qBox = Collapse.collapsible qButton qFields

valueTitle = text . Text.color blue . toText <| "Values"
valueTable = plainText "Lorem Ipsum"
(valueButton,valueCollapse) = Collapse.collapsibleSignal valueTitle valueTable
valueBox = Collapse.collapsible valueButton valueTable

xaxisKind = labelledChoice "X-axis" [("Linear",Linear),("Log",Log)]
yaxisKind = labelledChoice "Y-axis" [("Linear",Linear),("Log",Log)]


xaxis : Signal (Float->Float)
xaxis = lift4 axisMaker (snd xaxisKind) (lift toFloat width) (lift (\x -> x.qmin) qSignal) (lift (\x -> x.qmax) qSignal)
yaxis = lift4 axisMaker (snd yaxisKind) (lift toFloat height) (lift (foldr1 min . map snd)  plotPoints) (lift (foldr1 max . map snd) plotPoints)

xtics = lift2 (\kind pts -> ticMaker kind <| map snd pts) (snd xaxisKind) plotPoints

ytics = lift2 (\kind pts -> ticMaker kind <| map snd pts) (snd yaxisKind) plotPoints


plotPoints : Signal [(Float,Float)]
plotPoints = lift2 makePoints (lift qRange qSignal) (lift FormFactor.iq FormFactor.iqSignal)

-- Turns a list of point tuples into a 2d table
tablePoints pts = let xs = map (plainText . show . fst) pts
                      ys = map (plainText . show . snd) pts
                      xmax = maximum <| map widthOf xs
                      ymax = maximum <| map widthOf ys
                  in flow down <| map (\(x,y) -> Graphics.Element.width xmax x `beside` Graphics.Element.width ymax y) <| zip xs ys


scene terms = flow right <| terms

graphCanvas : Signal Element
graphCanvas = lift7 canvas width height xaxis (snd xaxisKind) yaxis (snd yaxisKind) plotPoints


width= lift (\x -> x - 300) Window.width
height= Window.height


sidebar : Signal Element
sidebar = lift FormFactor.formBox FormFactor.formCollapse `labove` lift qBox qCollapse `labove` constant  (text . bold . toText <| "Axis Options") `labove` fst xaxisKind `labove` fst yaxisKind -- `labove` lift (tablePoints . take 5) plotPoints


main = lift scene <| combine [graphCanvas , sidebar]
