{-# LANGUAGE RankNTypes #-}

import System.Random.MWC --This requires that the mwc-random

import Control.Monad
import Control.Monad.Primitive

-- A datatype that is used to store the current state of the evaluation of the 
-- Metropolis Hastings algorithm.
--
--   * currentState is the most resent state in the markov chain generated by
--     the algorithm
--
--   * currentValue is the value of the current state.
--
--   * accepts is the number of accepted state transition so far in the algorithm
data MarkovChain s = MarkovChain { currentState :: s
                                 , currentValue :: Double
                                 , accepts      :: Int
                                 }

-- A datatype that stores the candidate state and related data that is used by
-- the Metropolis Hastings algorithm.
--
--   * candidate is the proposed next state in the markov chain that is being
--     generated by the algorithm
--
--   * probabilities is the probabilities of going from the current state to
--     the candidate state and the probability of going from the candidate
--     state to current state.
data Candidate s = Candidate { candidate     :: s
                             , probabilities :: (Double, Double)
                             }

-- The type of the scoring function.
type Pi s = s -> Double

-- The type of the candidate generating function.
type Q m s = s -> Gen (PrimState m) -> m (Candidate s)


mHStep :: PrimMonad m       =>
          Pi s              -> -- The scoring function 
          Q m s             -> -- The candidate generating function
          MarkovChain s     -> -- The current state of the markov chain
          Gen (PrimState m) -> 
          m (MarkovChain s)
mHStep pi q state g = do
  let (x, px, acpts) = (currentState state, currentValue state, accepts state)
  (Candidate y (qxy, qyx)) <- q x g
  let py = pi y
  let a = min 1.0 ((qyx * py) / (qxy * px))
  u <- uniformR (0.0, 1.0) g
  if u <= a
    then return $ MarkovChain y py (acpts + 1)
    else return state


metropolisHastings :: PrimMonad m       =>
                      Int               -> -- The number of iterations
                      Pi s              -> -- The scoring function function
                      Q m s             -> -- The candidate generating function
                      s                 -> -- The initial state
                      Gen (PrimState m) ->
                      m ([s], Int)       
metropolisHastings n p q s g = f n (MarkovChain s (p s) 0)
  where
    f 0 (MarkovChain x _ accpts) = return ([x], accpts)
    f n s@(MarkovChain x _ _) = do
      t <- mHStep p q s g
      (xs, accpts) <- f (n-1) t
      return (x:xs, accpts)


p :: Pi Double
p x = exp (-(x^2) / 2) / (sqrt (2 * pi))

q :: PrimMonad m => Q m Double
q x g = do
  u <- uniformR (-0.5, 0.5) g
  return $ Candidate (x+u) (1.0, 1.0)

example :: Int -> Double -> GenIO -> IO ([Double], Int)
example n x = metropolisHastings n p q x