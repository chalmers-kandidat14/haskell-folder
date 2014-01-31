
import System.Random
import Control.Monad.State


type Rnd s = State StdGen s
type Q   s = s -> Rnd (s, Float, Float)
type P   s = s -> Rnd Float


randomSt :: Random a => Rnd a
randomSt = state random

randomRSt :: Random a => (a,a) -> Rnd a
randomRSt = state . randomR

metro :: P s -> Q s -> s -> Rnd [s]
metro p q x = do
  u  <- randomRSt (0.0, 1.0)
  (y, p1, p2) <- q x
  px <- p x
  py <- p y
  let a = min 1 $ (p2 * py) / (p1 * px)
  xs <- metro p q $ if u <= a then y else x
  return $ x : xs


