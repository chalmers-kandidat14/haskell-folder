module Energy (energy, Residue(..)) where

import qualified Data.Vector as V
import Data.Maybe (catMaybes)
import Chain
import Coord

data Residue = H | P

energy :: (Coord a) => Chain a -> V.Vector Residue -> Int
energy ch res = V.ifoldl f 0 res
    where
        f acc i x = acc + ( foldr ((+) . (residueEnergy x)) 0 . 
                    map (res V.!) . 
                    catMaybes . 
                    map (cIndex ch) $ 
                    validNeighbors ch i )
        
validNeighbors :: (Coord a) => Chain a -> Int -> [a]
validNeighbors ch i = filter (notNextTo ch i) (neighbors (ch!i))

notNextTo :: (Coord a) => Chain a -> Int -> a -> Bool
notNextTo ch i coord = all (/= coord) . catMaybes $ [ch!?(i-1), ch!?(i+1)]

residueEnergy :: Residue -> Residue -> Int
residueEnergy H H = -1
residueEnergy _ _ = 0
