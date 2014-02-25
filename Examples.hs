module Examples where

import Coord
import Chain
import Moves
import Print
import Energy
import qualified Data.Vector as V
import Metro
import Control.Monad.Primitive
import System.Random.MWC

myChain :: Chain Coord2d
myChain = fromList [Coord2d 1 1, Coord2d 1 2, Coord2d 2 2, Coord2d 3 2, 
                    Coord2d 3 1, Coord2d 4 1, Coord2d 4 2, Coord2d 4 3,
                    Coord2d 4 4, Coord2d 5 4, Coord2d 6 4, Coord2d 6 3,
                    Coord2d 7 3, Coord2d 8 3, Coord2d 9 3, Coord2d 9 4,
                    Coord2d 8 4, Coord2d 7 4, Coord2d 7 5, Coord2d 6 5,
                    Coord2d 5 5, Coord2d 4 5, Coord2d 3 5, Coord2d 2 5]

myResidues :: V.Vector HPResidue
myResidues = V.fromList $ concat $ [H] : 
                        (replicate 7 [H, P, P] ) ++
                        [[H, H]]

score :: (Coord a) => Chain a -> Double
score ch = exp $ - (energy myResidues ch)

main' n = withSystemRandom $ \g -> 
            metropolisHastings n score generateCandidate myChain g >>=
            \(xs, i) -> do
            printHP myResidues $ last xs
            putStrLn (show $ last xs)
            print i
            print (energy myResidues $ last xs)

main = main' 10000
