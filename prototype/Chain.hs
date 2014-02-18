{-# LANGUAGE GADTs #-}

module Chain where

import qualified Data.Vector as V
import qualified Data.Set as S

class (Eq a, Ord a) => Coord a where
    adj :: a -> a -> Bool
    dadj :: a -> a -> Bool
    neighbors :: a -> a -> [(a,a)]

data Chain a where
    Chain :: (Coord a) => 
        (V.Vector a) ->        -- A list of our coordinates in chain order
        S.Set a      ->        -- A set of our coords
        Chain a

toVector :: Chain a -> V.Vector a
toVector (Chain v _) = v

fromVector :: Coord a => V.Vector a -> Chain a
fromVector v = Chain v (S.fromList . V.toList $ v)
    

-- Public functions
cReverse :: Coord a => Chain a -> Chain a
cReverse = fromVector . V.reverse . toVector

(!) :: Chain a -> Int -> a
(!) ch i = (toVector ch) V.! i 

replace :: Coord a => Chain a -> Int -> [a] -> Chain a
replace ch i diff = fromVector $ (toVector ch) V.// (zip [i..] diff)

cEmpty :: Chain a -> a -> Bool
cEmpty (Chain _ sorted) element = not $ element `S.member` sorted

data Coord2d = Coord2d {xCoord :: Int, yCoord :: Int} deriving (Ord, Eq)

instance Coord Coord2d where
    adj a b  = abs (xCoord a - xCoord b) + abs (yCoord a - yCoord b) == 1
    dadj a b = abs (xCoord a - xCoord b) == 1 && abs (yCoord a - yCoord b) == 1
    neighbors (Coord2d ax ay) (Coord2d bx by)
                | ax == bx = [ (Coord2d (ax+1) ay, Coord2d (bx+1) by),
                               (Coord2d (ax-1) ay, Coord2d (bx-1) by) ]
                | ay == by = [ (Coord2d ax (ay+1), Coord2d bx (by+1)),
                               (Coord2d ax (ay-1), Coord2d bx (by-1)) ]

