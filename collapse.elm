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
      newVal = if value > goal then value - 1
               else if value < goal
                    then value + 1
                    else value
  in newVal

tuple a b = (a,b)

collapsible : String -> Element -> Signal Element
collapsible titleString es = let (title,titleButton) = Graphics.Input.customButton 
                                                       (plainText titleString)
                                                       (plainText titleString)
                                                       (plainText titleString)
                                 box = flow down [title,es]
                                 slider = Automaton.state (heightOf box) (move2 (heightOf title,heightOf box))
                                 clickTimer = lift2 tuple (signalFlipper titleButton)
                                              (Time.every (15 * millisecond))
                             in lift (\h -> container (widthOf box) h topLeft box)
                                <| Automaton.run slider (heightOf box) clickTimer


flipper : a -> Bool -> (Bool,Bool)
flipper _ state = (not state, not state)

signalFlipper : Signal a -> Signal Bool
signalFlipper = Automaton.run (Automaton.hiddenState False flipper) False     

main = lift (flow right) <| combine [collapsible "Hello" segment]
