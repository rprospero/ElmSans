module FormFactor where

import Util (labelledField,getFloat)
import Collapse
import Graphics.Input (fields)
import Fields (updater,fieldMaker)


hardSphere params q = 
           let xrad = q * params.radius
               bes  = 3.0*(sin xrad - xrad * cos xrad)/(xrad ^ 3)
               vol  = 4.0*pi/3.0*params.radius^3
               f = bes*params.drho
               f2 = f*f*vol*10^8
           in  params.scale*f2+params.bgd


hsCondenser (name,val) rec = case name of
                          "Scale" -> {rec - scale | scale = getFloat val}
                          "Radius" -> {rec - radius | radius = getFloat val}
                          "drho" -> {rec - drho | drho = getFloat val}
                          "Background" -> {rec - bgd | bgd = getFloat val}
                          _ -> rec

hsgroup = fields ("Name","1.0")

hsSignal = updater hsCondenser {scale=1.0,radius=1.0,drho=1.0,bgd=0.0} hsgroup.events

hsTitle = text . bold . Text.color (Color 0 0 255 1) . toText <| "HardSpheres"

hsLabelWidth = widthOf . plainText . show <| "Background"

hsFields = flow down <| [fieldMaker hsgroup hsLabelWidth "Scale" "1.0",
                         fieldMaker hsgroup hsLabelWidth "Radius" "1.0",
                         fieldMaker hsgroup hsLabelWidth "drho" "1.0",
                         fieldMaker hsgroup hsLabelWidth "Background" "0.0"]

(hsButton,hsCollapse) = Collapse.collapsibleSignal hsTitle hsFields
hsBox = Collapse.collapsible hsButton hsFields
