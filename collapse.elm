import Automaton
import Graphics.Element
import Graphics.Input
import Mouse
import Time
import Signal

(title,titleButton) = Graphics.Input.customButton (plainText "Hello") (plainText "Hello") (plainText "Hello")

segment = flow down [title,
                     plainText "World!",
                     plainText "I love",
                     plainText "Stacy"]

dropper : Int -> Element
dropper h = container (widthOf segment) h topLeft segment

dbox : Int -> Element
dbox h = flow down [dropper h, plainText "Next"]

move2 : (Int,Int) -> (Bool,Time.Time) -> Int -> Int
move2 (bottom,top) (click,_) value =
  let goal = if click then top else bottom
      newVal = if value > goal then value - 1
               else if value < goal
                    then value + 1
                    else value
  in newVal

tuple a b = (a,b)

clickTimer = lift2 tuple (signalFlipper titleButton) (Time.every (15 * millisecond))

slider : Automaton.Automaton (Bool,Time.Time) Int
slider = Automaton.state 10 (move2 (heightOf title,heightOf segment))

flipper : a -> Bool -> (Bool,Bool)
flipper _ state = (not state, not state)

signalFlipper : Signal a -> Signal Bool
signalFlipper = Automaton.run (Automaton.hiddenState False flipper) False     

main = lift (flow right) <| combine [lift dbox (Automaton.run slider 100 clickTimer),
                                     lift (plainText . show) clickTimer,
                                     lift (plainText . show) <| signalFlipper titleButton]
