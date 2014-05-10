module Metro (
               Candidate (..)
             , metropolisHastings
             ) where

import System.Random.MWC --This requires that the mwc-random
import Control.Monad
import Control.Monad.Primitive
import Debug.Trace

-- A function that takes two states and one parameter and
-- calculates the quotient between the scores of the two
-- states given the parameter.
type ScoreFunc s p = p -> s -> s -> Double

--  A function that generates a new random candidate
-- given the current state of a Markov chain.
type CandFunc s m = s -> m (Candidate s)

-- The data structure that is sent with the metropolis fold
data MetroState s = MetroState {
                                 currState :: [s]
                               , bestState :: [s]
                               }
updateMetroState :: (s -> Double) -> s -> MetroState s -> MetroState s
updateMetroState scoref st oldState = if bestScore < newScore
                                      then MetroState [st] [bestSt]
                                      else MetroState [st] [st]
       where
           bestSt = head $ bestState oldState
           bestScore = scoref bestSt
           newScore = scoref st

-- A structure containing the candidate state, the
-- probability of getting this state given the previous
-- state and the probability of getting the previous state
-- given this state.
data Candidate s = Candidate { candidate :: s
                             , pthere :: Double
                             , pback :: Double
                             }

metropolisHastings :: (PrimMonad m) =>
                   ScoreFunc s p ->
                   CandFunc s m ->
                   Gen (PrimState m) ->
                   s ->
                   [p] ->
                   m [s]
metropolisHastings scoref candf g init params = liftM currState $ foldM f start params
    where
        start = MetroState [init] [init]
        f marx t = do
            let state = head $ currState marx
            cand <- candf state
            let candstate = candidate cand
            let a = min 1.0 ((pback cand / pthere cand) * scoref t candstate state)
            let b = traceShow a a
            if a>=1
                then return (updateMetroState (scoref t state) candstate marx)
                else do
                    u <- uniformR (0.0, 1.0) g
                    if u <= a
                        then return (updateMetroState (scoref t state) candstate marx)
                        else return marx
