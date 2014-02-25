module Main (main) where

import Coord
import Chain
import Moves
import Print
import Energy
import Metro

import System.Random.MWC
import Control.Monad.Primitive
import qualified Data.Vector as V
import System.Environment
import Data.Char (isDigit)

-- Get probability for one list element given uniform distribution
getProb :: [a] -> Double
getProb list = fromRational $ 1 / (fromIntegral $ length list)

-- Randomly select one candidate of all given pullMoves
generateCandidate :: (PrimMonad m)    => 
                    Chain Coord2d     -> 
                    Gen (PrimState m) -> 
                    m (Candidate (Chain Coord2d))
generateCandidate ch gen = do 
    let list = pullMoves ch
    chosenMove <-  pick list gen
    let candidate = after chosenMove
    let prob = getProb list
    let probBack = getProb $ pullMoves candidate
    return $ Candidate candidate (prob, probBack)

-- Create a list of residues from an input string
createResidues :: String -> [HPResidue]
createResidues [] = []
createResidues (x:[]) = [read [x]]
createResidues (x:y:xs) = if isDigit y
                          then (replicate (read [y]) (read [x])) ++ createResidues xs
                          else (read [x]) : (createResidues (y:xs))

-- Generate a chain with a fixed length
createChain :: Int -> Chain Coord2d
createChain n = fromList [Coord2d 1 x | x <- [1..n]]


-- We calculate the score with e^(-E)
score :: (Coord a) => Chain a -> Double
score ch = undefined -- exp $ - (energy myRes ch)

run :: String -> String -> IO ()
run input iterations = do    
    let residues = V.fromList $ createResidues input
    let chain = createChain (V.length residues)
    let score ch = exp $ - (energy residues ch)
    run' score chain residues (read iterations)

-- Run the algorithm!
run' score chain residues n = withSystemRandom $ \g -> 
            metropolisHastings n score generateCandidate chain g >>=
            \(xs, i) -> do
            printHP residues $ last xs
            putStrLn "----------------------------"
            putStrLn (show $ last xs)
            putStrLn $ "Number of accepted transitions: " ++ (show i)
            putStrLn $ "Final energy: " ++ show (energy residues $ last xs)

main :: IO ()
main = do
    (input:iterations:_) <- getArgs
    run input iterations
