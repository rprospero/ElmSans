module Util where

import String
import Graphics.Input


--Helper Function to read a Float
getFloat = fromJust 0 . String.toFloat

liftFloat : Signal String -> Signal Float
liftFloat = lift getFloat

labelit title elem = flow right [plainText title, elem]

labelledField : String -> String-> (Signal Element, Signal Float)
labelledField title dflt = let (elem,str) = Graphics.Input.field dflt
  in (lift (labelit title) elem,liftFloat str)


fromJust dflt val = maybe dflt id val
