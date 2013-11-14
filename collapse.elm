import Automaton
import Graphics.Element
import Mouse

title = plainText "Hello"

segment = flow down [title, plainText "World!", plainText "I love", plainText "Stacy"]

dropper h = height h segment

dbox h = flow down [dropper h, plainText "Next"]

heightManager click = if click then 100 else 10

flipper : Automaton.Automaton Bool Bool
flipper = Automaton.state False (\x y -> if x then not y else y) 

main = lift (dbox . heightManager) (Automaton.run flipper False Mouse.isDown)