module CoreShell where

import Graphics.Input (fields)
import Fields (fieldMaker,updater)
import Util (getFloat)

type CoreParam = {bgd : Float,
                  coreRadius : Float,
                  shellThickness : Float,
                  coreSLD : Float,
                  shellSLD : Float,
                  solventSLD : Float,
                  scale : Float}

coreShell : CoreParam -> Float -> Float
coreShell params q = 
  let rc = params.coreRadius
      t = params.shellThickness
      rs = rc + t
      vc = 4.0*pi/3.0*rc^3
      vs = 4.0*pi/3.0*rs^3
      j = \x -> (sin x - (x * cos x))/(x^3)
  in
   params.scale/vc * (3 * vc * (params.coreSLD - params.shellSLD)*j(q*rc)/(q*rc)
                     +
                     3 * vs * (params.shellSLD - params.solventSLD)*j(q*rs)/(q*rs)) + params.bgd

csCondenser (name,val) rec = case name of
                             "Scale" -> {rec - scale | scale = getFloat val}
                             "Core Radius" -> {rec - coreRadius | coreRadius = getFloat val}
                             "Shell Thickness" -> {rec - shellThickness | shellThickness = getFloat val}
                             "Core SLD" -> {rec - coreSLD | coreSLD = getFloat val}
                             "Shell SLD" -> {rec - shellSLD | shellSLD = getFloat val}
                             "Solvent SLD" -> {rec - solventSLD | solventSLD = getFloat val}
                             "Background" -> {rec - bgd | bgd = getFloat val}


csgroup = fields ("Name","1.0")

csSignal = updater csCondenser {scale=1.0,
                                coreRadius=60.0,
                                shellThickness=10.0,
                                coreSLD = 10^(-6),
                                shellSLD = 2*10^(-6),
                                solventSLD = 3*10^(-6),
                                bgd=0.0} csgroup.events


csLabelWidth : Int
csLabelWidth = widthOf . plainText . show <| "ShellThickness"

csFields = flow down <| [fieldMaker csgroup csLabelWidth "Scale" "1.0",
                         fieldMaker csgroup csLabelWidth "Core Radius" "60.0",
                         fieldMaker csgroup csLabelWidth "Shell Thickness" "10.0",
                         fieldMaker csgroup csLabelWidth "Core SLD" "1.0e-6",
                         fieldMaker csgroup csLabelWidth "Shell SLD" "2.0e-6",
                         fieldMaker csgroup csLabelWidth "SolventSLD" "3.0e-6",
                         fieldMaker csgroup csLabelWidth "Background" "0.0"]
