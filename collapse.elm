import Automaton
import Graphics.Element
import Mouse
import Time
import Signal

title = plainText "Hello"

segment = flow down [title, plainText "World!", plainText "I love", plainText "Stacy"]

dropper h = height h segment

dbox h = flow down [dropper h, plainText "Next"]

move2 : (Int,Int) -> (Bool,Time.Time) -> (Bool,(Bool,Int)) -> ((Bool,(Bool,Int)),(Bool,Int))
move2 (bottom,top) (click,_) (oldclick,(stt,value)) =
  let goalState = if (click && (not oldclick)) then not stt else stt
      goal = if goalState then top else bottom
      newVal = if value > goal then value - 1
               else if value < goal
                    then value + 1
                    else value
  in ((click,(goalState,newVal)),(goalState,newVal))

tuple a b = (a,b)

clickTimer = lift2 tuple (Signal.dropRepeats Mouse.isDown) (Time.every (15 * millisecond))

slider : Automaton.Automaton (Bool,Time.Time) (Bool,Int)
slider = Automaton.hiddenState (False,(False,0)) (move2 (100,10))


main = lift (flow right) <| combine [lift (dbox . snd) (Automaton.run slider (False,100) clickTimer), lift (plainText . show) clickTimer]
