module Chain ( 
               Chain
             , fromList
             , toList
             , cReverse
             , cEmpty
             , cLength
             , (!)
             , replace
             ) where

import qualified Data.Vector as V
import qualified Data.Set as S
import Coord

data Chain a = Chain (V.Vector a)          -- A list of our coordinates in chain order
                     (S.Set a)             -- A set of our coords
                     deriving (Eq)

toVector :: Chain a -> V.Vector a
toVector (Chain v _) = v

fromVector :: (Ord a) => V.Vector a -> Chain a
fromVector v = Chain v (S.fromList . V.toList $ v)
    

-- Public functions

toList :: Chain a -> [a]
toList = V.toList . toVector

fromList :: Ord a => [a] -> Chain a
fromList = fromVector . V.fromList

cReverse :: Ord a => Chain a -> Chain a
cReverse = fromVector . V.reverse . toVector

(!) :: Chain a -> Int -> a
(!) ch i = (toVector ch) V.! i 

cLength :: Chain a -> Int
cLength = V.length . toVector

replace :: (Ord a) => Chain a -> Int -> [a] -> Chain a
replace ch i diff = fromVector $ (toVector ch) V.// (zip [i..] diff)

cEmpty :: (Ord a) => Chain a -> a -> Bool
cEmpty (Chain _ sorted) element = not $ element `S.member` sorted
