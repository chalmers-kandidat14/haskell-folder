module Energy (energy, HPResidue(..)) where

import qualified Data.Vector as V
import Data.Maybe (catMaybes, mapMaybe)
import Chain
import Coord

class NeighborResidue n where
    residueEnergy :: n -> n -> Int

data HPResidue = H | P deriving (Show, Read)

instance NeighborResidue HPResidue where
    residueEnergy H H = -1
    residueEnergy _ _ = 0

energy :: (Coord a, NeighborResidue n) => V.Vector n -> Chain a -> Double
energy res ch = fromIntegral $ V.ifoldl f 0 res
    where
        f acc i x = acc + ( foldr ((+) . residueEnergy x . (res V.!)) 0 $ 
                    mapMaybe (cIndex ch) $ 
                    validNeighbors ch i )
        
validNeighbors :: (Coord a) => Chain a -> Int -> [a]
validNeighbors ch i = filter (notNextTo ch i) (neighbors (ch!i))

notNextTo :: (Coord a) => Chain a -> Int -> a -> Bool
notNextTo ch i coord = notElem coord . catMaybes $ [ch!?(i-1), ch!?(i+1)]

