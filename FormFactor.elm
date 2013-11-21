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
           in  params.scale*f2+params.background


hardSphereParams scale drho radius background =
                 {scale=scale,drho=drho,
                 radius=radius,
                 background=background}


rad = labelledField "Radius" "0.2"
scale = labelledField "Scale" "1.0"
drho = labelledField "drho" "0.000001"
bgd = labelledField "Background" "0.000001"


hsCondenser (name,val) rec = case name of
                          "Scale" -> {rec - scale | scale = getFloat val}
                          "Radius" -> {rec - radius | radius = getFloat val}
                          "drho" -> {rec - drho | drho = getFloat val}
                          "Background" -> {rec - bgd | bgd = getFloat val}
                          _ -> rec

hardParams = lift4 hardSphereParams 
           (snd scale) (snd drho) (snd rad) (snd bgd)

hardBox = foldl (lift2 above) (fst rad) (map fst [scale, drho, bgd])

hsgroup = fields ("Name","1.0")

hsSignal = updater hsCondenser {scale=1.0,radius=1.0,drho=1.0,bgd=0.0} hsgroup.events

hsTitle = plainText "HardSpheres"

hsFields = flow down <| [fieldMaker hsgroup "Scale" "1.0",
                         fieldMaker hsgroup "Radius" "1.0",
                         fieldMaker hsgroup "drho" "1.0",
                         fieldMaker hsgroup "Background" "0.0"]

(hsButton,hsCollapse) = Collapse.collapsibleSignal hsTitle hsFields
hsBox = Collapse.collapsible hsButton hsFields



