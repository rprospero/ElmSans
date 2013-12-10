module FormFactor where

import HardSphere
import Util (labelledField,getFloat)
import Collapse
import Graphics.Input (fields,dropDown)
import Fields (updater,fieldMaker)

data Form = HSphere

data Param = HSParam HardSphere.HardParam

formTitle : Element
formTitle = text . Text.color (Color 0 0 255 1) . toText <| "▼ Form Factor"

(formButton,formCollapse) = Collapse.collapsibleSignal formTitle HardSphere.hsFields

(formChoice,formSignal) = dropDown [("Hard Spheres",HSphere)]

iq : Form -> Param -> Float  -> Float
iq form param = case form of
               HSphere -> case param of
                 HSParam p -> HardSphere.hardSphere p

iqSignal = (lift HSParam) HardSphere.hsSignal

formBox : Form -> Int -> Element
formBox form = Collapse.collapsible formButton <| case form of
  HSphere -> HardSphere.hsFields
