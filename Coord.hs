module Coord where

class (Eq a, Ord a) => Coord a where
    adj :: a -> a -> Bool
    dadj :: a -> a -> Bool
    neighbors :: a -> [a]
    neighborPairs :: a -> a -> [(a,a)]

data Coord2d = Coord2d {xCoord :: Int, yCoord :: Int} deriving (Ord, Eq)

instance Show Coord2d where
   show (Coord2d x y) = "(" ++ show x ++ "x" ++ show y ++ ")"

instance Coord Coord2d where
    adj a b  = abs (xCoord a - xCoord b) + 
               abs (yCoord a - yCoord b) == 1

    dadj a b = abs (xCoord a - xCoord b) == 1 && 
               abs (yCoord a - yCoord b) == 1
    
    neighbors (Coord2d x y) = [Coord2d (x+1) y, Coord2d x (y+1)]

    neighborPairs (Coord2d ax ay) (Coord2d bx by)
                | ax == bx = [ (Coord2d (ax+1) ay, Coord2d (bx+1) by),
                               (Coord2d (ax-1) ay, Coord2d (bx-1) by) ]
                | ay == by = [ (Coord2d ax (ay+1), Coord2d bx (by+1)),
                               (Coord2d ax (ay-1), Coord2d bx (by-1)) ]
