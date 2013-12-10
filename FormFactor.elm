module FormFactor where

import HardSphere
import Util (labelledField,getFloat)
import Collapse
import Graphics.Input (fields)
import Fields (updater,fieldMaker)


formTitle : Element
formTitle = text . Text.color (Color 0 0 255 1) . toText <| "▼ Form Factor"

(formButton,formCollapse) = Collapse.collapsibleSignal formTitle HardSphere.hsFields

iq = HardSphere.hardSphere

iqSignal = HardSphere.hsSignal

formBox = Collapse.collapsible formButton HardSphere.hsFields
