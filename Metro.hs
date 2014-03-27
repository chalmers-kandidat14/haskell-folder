{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Metro (
               Candidate (..)
             , metropolisHastings
             , pick
             ) where

import System.Random.MWC --This requires that the mwc-random
import Debug.Trace
import Control.Monad
import Control.Monad.Primitive

-- A datatype that is used to store the current state of the evaluation of the 
-- Metropolis Hastings algorithm.
--
--   * currentState is the most resent state in the markov chain generated by
--     the algorithm
--
--   * currentValue is the value of the current state.
data Markov a m s = (CandGen a m s) =>
                  Markov { chain :: [s]
                         , nxtCand :: Candidate s
                         , candGen :: a m s
                         , candFun :: CandFunc a m s
                         , markovLength :: Int
                         }

initMarkov :: PrimMonad m => s -> CandFunc m s -> m (Markov a m s)
initMarkov state f = do
                        let candgen = f state
                        cand <- genCand candgen 
                        return $ Markov [state] cand candgen f 1

currState :: Markov a m s -> s
currState = head . chain

discCand :: Markov a m s -> m Markov a m s
discCand (Markov ch _ g f l) = do 
                                newcand <- genCand g
                                return $ Markov ch newcand g f l

pickCand :: PrimMonad m => Markov m s -> m (Markov a m s)
pickCand (Markov ch cand g f l) = do
                                    let newstate = candidate cand
                                    let newgen = f newstate
                                    newcand <- genCand newgen
				    return $ Markov (newstate:ch) newcand newgen f (l+1)

-- Select one element at random from a list
pick :: (PrimMonad m) => [a] -> Gen (PrimState m) -> m a
pick xs gen = uniformR (0, (length xs)-1) gen >>= return . (xs!!)

-- The type of the scoring function.
type ScoreFunc s p = s -> s -> p -> Double

class CandGen s m where
    genCand :: a m s ->  m (Candidate s)

-- The type of the candidate generating function.
type CandFunc a m s  = CandGen a m s => s -> a m s

data Candidate s = Candidate { candidate :: s
                             , pthere :: Double
                             , pback :: Double
                             }

mHStep :: PrimMonad m       =>
          ScoreFunc s p     -> -- The scoring function 
          Gen (PrimState m) ->
          Markov a m s        ->
          p                 -> -- The remaining temperature schedule
          m (Markov m s)
mHStep scoref g marx t = do
     let cand = nxtCand marx
     let state = currState marx
     let a = min 1.0 ((pback cand / pthere cand) * scoref (candidate cand) state t)
     u <- uniformR (0.0, 1.0) g
     if a >= 1 || u <= a
       then pickCand marx
       else discCand marx

metropolisHastings :: (PrimMonad m) =>
                   ScoreFunc s p ->
                   CandFunc a m s ->
                   Gen (PrimState m) ->
                   s ->
                   [p] ->
                   m [s]
metropolisHastings scoref candf gen init params = do 
    let step = (mHStep scoref gen)
    initChain <- (initMarkov init candf)
    markovchain <- foldM step initChain params
    return $ chain markovchain
