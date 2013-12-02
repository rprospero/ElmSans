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

labelledChoice : String -> [(String,a)] -> (Signal Element,Signal a)
labelledChoice title vals =
               let (elem,val) = Graphics.Input.dropDown vals
               in (lift (labelit title) elem, val)


fromJust dflt val = maybe dflt id val

range a b count = map (\x -> a + x/(count-1) *(b-a)) [0..(count)]


labove = lift2 above

floatPrecision : Int -> Float -> String
floatPrecision digits x =  let splitup = (String.split "e" <| show x)
                               mantissa = getFloat <| head splitup
                               adjust = if mantissa < 1 then 1 else 0
                               exponent = if length splitup == 1 then 0 else getFloat . head  <| tail splitup
                               secondExponent = truncate . logBase 10 <| mantissa
                               sigDigs = round (mantissa  / (10 ^ (toFloat (secondExponent - digits))))
                               final = (toFloat sigDigs) / (10 ^ (toFloat digits)) * (10^adjust)
                           in String.join "e" [show final , show (secondExponent-1)]
                               
