import Util (getFloat)
import Graphics.Input (fields,emptyFieldState,FieldState)
import Automaton


group = fields ("Name",1.0)

sig = group.events

fieldMaker : {events:Signal (String,a),field:(FieldState->(String,a)) -> String -> FieldState ->Element } -> (String -> a) -> String -> a -> Element
fieldMaker group f name dflt = group.field 
                               (\r -> (name,f r.string))
                               name {string=show dflt,selectionStart=0,selectionEnd=0}

condenser (name,val) rec = case name of
                "Test" -> {rec - test | test = val}
                "Foo" -> {rec - foo | foo = val}
                _ -> rec

updater : ((String,a) -> b -> b) -> b -> Signal (String,a) -> Signal b
updater f base = Automaton.run (Automaton.state base f) base

main = lift (flow down) <| combine [lift (plainText . show) <| updater condenser {test=0,foo=0} sig,
                                    constant <| fieldMaker group getFloat "Test" 3.14,
                                    constant <| fieldMaker group getFloat "Foo" 159.1]
