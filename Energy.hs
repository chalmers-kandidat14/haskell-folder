module Energy where

import Data.Vector
import Chain
import Coords

data Residue = H | P

energy :: Chain -> Vector Residue -> Int
energy ch res = ifoldl f 0 res
    where
        f acc i x = map (res!) . map ch $ filter (notNextTo i) (neighbors (ch!i))
        notNextTo i coord = not $ ch!(i-1) /= coord || ch!(i+1) /= coord

residueEnergy :: Residue -> Residue -> Int
residueEnergy H H = -1
residueEnergy _ _ = 0
