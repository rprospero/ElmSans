module FormFactor where

import Util (labelledField,getFloat,getAssoc)
import Collapse
import Graphics.Input (fields,dropDown)
import Fields (updater,fieldMaker)

import HardSphere
import CoreShell


data Form = HSphere | CShell

data Param = HSParam HardSphere.HardParam | CSParam CoreShell.CoreParam

formSignals : Signal [(Form,Param)]
formSignals = combine [lift HSParam HardSphere.hsSignal,
                       lift CSParam CoreShell.csSignal] |> lift2 (zipWith Collapse.tuple) (constant [HSphere,CShell])

formTitle : Element
formTitle = text . Text.color (Color 0 0 255 1) . toText <| "▼ Form Factor"

(formButton,formCollapse) = Collapse.collapsibleSignal formTitle .
                            maximum . map heightOf <|
                            [HardSphere.hsFields,CoreShell.csFields]

(formChoice,formSignal) = dropDown [("Hard Spheres",HSphere),("Core Shell",CShell)]

iq : Form -> Param -> Float  -> Float
iq form param = case form of
               HSphere -> case param of
                 HSParam p -> HardSphere.hardSphere p
               CShell -> case param of
                 CSParam p -> CoreShell.coreShell p

iqSignal = lift2 getAssoc formSignal formSignals

formBox : Form -> Int -> Element
formBox form = Collapse.collapsible formButton <| case form of
  HSphere -> HardSphere.hsFields
  CShell -> CoreShell.csFields
