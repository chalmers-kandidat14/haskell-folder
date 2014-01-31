
import System.Random


type Q s = (StdGen -> s -> (s, Float, Float, StdGen))
type P s = (s -> Float)

metro :: StdGen -> s -> P s -> Q s -> [s]
metro s x p q = x : metro s'' (if u <= a then y else x) p q
  where
    (u, s')          = randomR (0.0, 1.0) s
    (y, p1, p2, s'') = q s' x
    a                = min (((p y) * p2) / ((p x) * p1)) 1
