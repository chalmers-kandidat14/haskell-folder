
{-
 Module for assessing similarity between two chains
-}

module Similarity (similarity) where

import Main
import Chain
import HPModel
import Coord

import Data.List (intersect)
import Data.Maybe (mapMaybe, catMaybes)

similarity :: Coord a => [HPResidue] -> Chain a -> Chain a -> Int
similarity res ch ch' = foldr f 0 $ zip a b 
    where
        f (xs,ys) acc = acc + (sum (overlap xs ys)) 
        
        -- We intersect with the indices of the H residues
        -- so we only check the connections of the H
        overlap xs ys = xs `intersect` ys `intersect` validIndices
       
        validIndices = map snd $
                       filter (isHydrophobic . fst) $ 
                       zip res [0..]

        connections = sum (concat a)
        a = buildConnectionMatrix ch
        b = buildConnectionMatrix ch'

-- Builds a matrix with the the connections for each residue (row)
-- observe that each connection is only counted once, so the matrix
-- is not symmetric
buildConnectionMatrix :: (Coord a, Num b) => Chain a -> [[b]]
buildConnectionMatrix ch = map f [0..(n-1)] 
    where
        f = map fromIntegral . 
            mapMaybe (cIndex ch) . 
            validNeighbors ch
        n = cLength ch

-- Filter the chain of coordinates to only contain
-- those with a H-residue
filterHResidues :: [HPResidue] -> [a] -> [a]
filterHResidues rs xs = map snd $ filter f $ zip rs xs
    where
        f (res, x) = if isHydrophobic res 
                     then True
                     else False

validNeighbors :: (Coord a) => Chain a -> Int -> [a]
validNeighbors ch i = filter (notNextTo ch i) (neighbors (ch!i))

notNextTo :: (Coord a) => Chain a -> Int -> a -> Bool
notNextTo ch i coord = notElem coord . catMaybes $ [ch!?(i-1), ch!?(i+1)]
