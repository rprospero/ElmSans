module NumInt where

simpson' f a b epsilon estimate fa fb fc n = 
         let c = (a + b)/2
             h = b-a
             d = (a+c)/2
             e = (c+b)/2
             fd = f d
             fe = f e
             sleft = (h/12)*(fa+4*fd+fc)
             sright = (h/12)*(fc+4*fe+fb)
             s2 = sleft + sright
         in -- trace (show $ (a,b,s2-estimate,s2)) $
            if n == 0 || abs (s2-estimate) < (epsilon * 15) || abs (s2-estimate)/s2 < 0.0000001
            then (s2,abs(s2-estimate))
            else let left = simpson' f a c (epsilon/2) sleft fa fc fd (n-1)
                     right = simpson' f c b (epsilon/2) sright fc fb fe (n-1) 
                 in (fst left+fst right,snd right+snd right)

simpson n f a b = let c = (a+b)/2
                      h = b-a
                      fa = f a
                      fb = f b
                      fc = f c
                  in simpson' f a b 0.00001 (h/6*(fa + 4*fc + fb)) fa fb fc n

stretch f = \t -> (1-t)^(-2) * f (t/(1-t))

infinite n f = let f' = stretch f
                   fa = f' 0
                   fc = f' 0.5
                   fb = f' 0.9999 
               in simpson' f' 0 1 0.0000001 (1.0/6.0*(fa+4*fc+fb)) fa fb fc n

gaussian x = e ^ (-1*x^2)

main = asText . map (\(x,y) -> (x,y,abs (x-sqrt pi/2)/y,abs (x-sqrt pi/2))) . map (\x -> infinite x gaussian) . map (\x -> x * 1) <| [0..10]

-- main = asText . map (\(x,y) -> (x,y,abs (x-sqrt pi/2)/y)) . map (\x -> simpson x gaussian 0 20) . map (\x -> x * 1) <| [0..20]

-- main = asText <| map (stretch gaussian) [0,0.5,0.9,0.99,1]
