{-# LANGUAGE GADTs #-}

module Chain where

import qualified Data.Vector as V

class (Eq a, Ord a) => Coord a where
    adj :: a -> a -> Bool
    dadj :: a -> a -> Bool
    neighbors :: a -> a -> [(a,a)]

data Chain a where
    Chain :: (Coord a) => 
        (V.Vector a) ->        -- A list of our coordinates in chain order
        Maybe (V.Vector a) ->  -- A sorted list of our coords
        Chain a

toVector :: Chain a -> Vector a
toVector (Chain v _) = v

-- Public functions
cReverse :: Chain a -> Chain a
cReverse v = Chain (V.reverse . toVector v) Nothing

(!) :: Int -> Chain a -> a
(!) = undefined

cHead :: Chain a -> a
cHead = undefined

cTake :: Int -> Chain a -> Chain a
cTake = undefined

cDrop :: Int -> Chain a -> Chain a
cDrop = undefined

cTail :: Chain a -> Chain a
cTail = undefined

cEmpty :: Chain a -> a -> Bool
cEmpty = undefined

data Coord2d = Coord2d (Int, Int) deriving (Ord, Eq)

instance Coord Coord2d where
    adj _ _ = True
    dadj _ _ = True
    neighbors a b = [(a,b)]

