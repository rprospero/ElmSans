module FormFactor where

import Util (labelledField)

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


hardParams = lift4 hardSphereParams 
           (snd scale) (snd drho) (snd rad) (snd bgd)

hardBox = foldl (lift2 above) (fst rad) (map fst [scale, drho, bgd])