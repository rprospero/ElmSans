module FormFactor where

import HardSphere
import Util (labelledField,getFloat,getAssoc)
import Collapse
import Graphics.Input (fields,dropDown)
import Fields (updater,fieldMaker)

data Form = HSphere | CoreShell

data Param = HSParam HardSphere.HardParam | CSParam Int

formSignals : Signal [(Form,Param)]
formSignals = combine [lift HSParam HardSphere.hsSignal,constant (CSParam 0)] |> lift2 (zipWith Collapse.tuple) (constant [HSphere,CoreShell])

formTitle : Element
formTitle = text . Text.color (Color 0 0 255 1) . toText <| "▼ Form Factor"

(formButton,formCollapse) = Collapse.collapsibleSignal formTitle HardSphere.hsFields

(formChoice,formSignal) = dropDown [("Hard Spheres",HSphere),("Core Shell",CoreShell)]

iq : Form -> Param -> Float  -> Float
iq form param = case form of
               HSphere -> case param of
                 HSParam p -> HardSphere.hardSphere p
               CoreShell -> case param of
                 CSParam p -> id

iqSignal = lift2 getAssoc formSignal formSignals

formBox : Form -> Int -> Element
formBox form = Collapse.collapsible formButton <| case form of
  HSphere -> HardSphere.hsFields
