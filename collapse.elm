module Collapse where

import Automaton
import Graphics.Element
import Graphics.Input
import Mouse
import Time
import Signal

segment = flow down [plainText "World!",
                     plainText "I love",
                     plainText "Stacy"]

move2 : (Int,Int) -> (Bool,Time.Time) -> Int -> Int
move2 (bottom,top) (click,_) value =
  let goal = if click then top else bottom
  in case compare value goal of
    GT -> value - 1
    LT -> value + 1
    EQ -> value

tuple a b = (a,b)

collapsible : Element -> Element -> Int -> Element
collapsible title es = let box = flow down [title,es]
                       in (\h -> container (widthOf box) h topLeft box)

makeTitle : String -> (Element, Signal ())
makeTitle titleString = Graphics.Input.customButton (plainText titleString) (plainText titleString) (plainText titleString)                          

slider : Int -> Int -> Automaton.Automaton (Bool,Time.Time) Int
slider hTitle hBox = Automaton.state hTitle (move2 (hTitle,hBox))

clickTimer : Signal () -> Signal (Bool,Time.Time)
clickTimer titleButton = lift2 tuple (signalFlipper titleButton)
                         (Time.every (15 * millisecond))

flipper : a -> Bool -> (Bool,Bool)
flipper _ state = (not state, not state)

signalFlipper : Signal a -> Signal Bool
signalFlipper = Automaton.run (Automaton.hiddenState False flipper) False     

collapsibleSignal : Element -> Element -> Signal () -> Signal Int
collapsibleSignal title body button = Automaton.run (slider (heightOf title)
                                                    (heightOf title + heightOf segment))
                                                    (heightOf title)
                                                    (clickTimer button)

(title,tb) = makeTitle "Hello"
main = lift (flow down) <| combine [lift (collapsible title segment) 
                                     <| collapsibleSignal title segment tb,
                                     constant <| plainText "Next"]
