import Graphics.Input
import Graphics.Collage
import String


labelit title elem = flow right [plainText title, elem]


labelledField title = let (elem,str) = Graphics.Input.field ""
  in (lift (labelit title) elem,str)


(radField,radius) = Graphics.Input.field "Radius"


fromJust dflt val = maybe dflt id val


fst (a,_) = a


--Helper Function to read a Float
getFloat = fromJust 0 . String.toFloat
        
rad = labelledField "Radius"
concentration = labelledField "Concentration"

canvas x y = Graphics.Collage.collage 200 200 [traced (solid lightBlue) <| path [(0,0),(getFloat x,getFloat y)]]

add a b = getFloat a + getFloat b


radcon = lift (plainText . show) <| lift2 add (snd rad) (snd concentration)


scene terms = flow down <| terms

main = lift scene <| combine [lift2 canvas (snd rad) (snd concentration), fst rad, fst concentration, radcon]
