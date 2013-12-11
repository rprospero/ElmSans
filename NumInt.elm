import Debug.Trace (trace)
import Foreign.C.Math.Double (j0)

simpson' f a b epsilon estimate fa fb fc = 
         let c = (a + b)/2
             h = b-a
             d = (a+c)/2
             e = (c+b)/2
             fd = f d
             fe = f e
             sleft = (h/12)*(fa+4*fd+fc)
             sright = (h/12)*(fc+4*fe+fb)
             s2 = sleft + sright
             left = simpson' f a c (epsilon/2) sleft fa fc fd
             right = simpson' f c b (epsilon/2) sright fc fe fb 
         in -- trace (show $ (a,b,s2-estimate,s2)) $
            if abs (s2-estimate) < max (epsilon * 15) 1e-7
            then (s2 + (s2-estimate)/15,abs(s2-estimate))
            else (fst left+fst right,snd right+snd right)

simpson f a b = let c = (a+b)/2
                    h = b-a
                    fa = f a
                    fb = f b
                    fc = f c
                in simpson' f a b 1e-5 (h/6*(fa + 4*fc + fb)) fa fb fc

stretch f = \t -> (1-t)**(-2) * f (t/(1-t))

infinite f = let f' = stretch f
                 fa = f 0
                 fc = f 0.5
                 fb = 0 --otherwise we aren't converging; this handles the NAN issue
             in simpson' f' 0 1 1e-5 (1.0/6.0*(fa+4*fc+fb)) fa fb fc

