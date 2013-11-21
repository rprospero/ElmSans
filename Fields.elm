module Fields where

import Util (getFloat)
import Graphics.Input (fields,emptyFieldState,FieldState)
import Automaton


group = fields ("Name","1.0")

sig = group.events

fieldMaker : {events:Signal (String,String),field:(FieldState->(String,String)) -> String -> FieldState ->Element } -> String -> String -> Element
fieldMaker group name dflt = group.field 
                             (\r -> (name,r.string))
                             name {string=dflt,selectionStart=0,selectionEnd=0}

condenser (name,val) rec = case name of
                "Test" -> {rec - test | test = getFloat val}
                "Foo" -> {rec - foo | foo = getFloat val}
                _ -> rec

updater : ((String,String) -> b -> b) -> b -> Signal (String,String) -> Signal b
updater f base = Automaton.run (Automaton.state base f) base

main = lift (flow down) <| combine [lift (plainText . show) <| updater condenser {test=3.14,foo=159.1} sig,
                                    constant <| fieldMaker group "Test" "3.14",
                                    constant <| fieldMaker group "Foo" "159.1"]
