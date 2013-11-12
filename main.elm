import Graphics.Input
import Graphics.Collage
import String


labelit title elem = flow right [plainText title, elem]


labelledField title = let (elem,str) = Graphics.Input.field ""
  in (lift (labelit title) elem,str)


(radField,radius) = Graphics.Input.field "Radius"


fromJust dflt val = maybe dflt id val


fst (a,_) = a


scene terms = flow down terms

--Helper Function to read a Float
getFloat = fromJust 0 . String.toFloat
        
rad = labelledField "Radius"
concentration = labelledField "Concentration"

add a b = getFloat a + getFloat b


radcon = lift (plainText . show) <| lift2 add (snd rad) (snd concentration)


main = lift scene <| combine [fst rad, fst concentration, radcon]
