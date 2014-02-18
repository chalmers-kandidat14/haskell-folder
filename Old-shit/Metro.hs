
module Metro where

import System.Random
import Control.Monad.State


type Rnd s = State StdGen s
type Q   s = s -> Rnd (s, Float, Float)
type P   s = s -> Rnd Float


randomSt :: Random a => Rnd a
randomSt = state random

randomRSt :: Random a => (a,a) -> Rnd a
randomRSt = state . randomR

metro :: Int ->P s -> Q s -> s -> Rnd [s]
metro 0 _ _ _ = return []
metro n p q x = do
  (y, qxy, qyx) <- q x
  px <- p x
  py <- p y
  let a = min 1.0 ((qyx * py) / (qxy * px))
  u  <- randomRSt (0.0, 1.0)
  liftM (x :) $ metro (n-1) p q $ if u <= a then y else x


q :: Q Float
q x = do
  u <- randomRSt (-0.5, 0.5)
  return (x+u, 1.0, 1.0)

p :: P Float
p x = return $ exp (- (x^2) / 2) / (sqrt (2 * pi)) 

