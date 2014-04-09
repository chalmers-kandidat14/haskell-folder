module Chain ( 
               Chain
             , fromList
             , toList
             , cReverse
             , cEmpty
             , cLength
             , cIndex
             , (!)
             , (!?)
             , replace
             ) where

import qualified Data.Vector as V
import qualified Data.Set as S
import Coord

newtype Memb a = M (a, Int)

instance (Ord a) => Ord (Memb a) where
    compare (M (a, _)) (M (b, _)) = compare a b

instance (Eq a) => Eq (Memb a) where
    (==) (M (a, _)) (M (b, _)) = a == b 

data Chain a = Chain (V.Vector a)          -- A list of our coordinates in chain order
                     (S.Set (Memb a))        -- A set of our coords and their indices
                     Boolean
                     deriving (Eq)

rix :: V.Vector a -> Int -> Int
rix v i = (V.length v - a) - 1


toVector :: Chain a -> V.Vector a
toVector (Chain v _ False) = v
toVector (Chain v _ True) = V.reverse v

fromVector :: (Ord a) => V.Vector a -> Chain a
fromVector v = Chain v (S.fromList . map M $ zip (V.toList v) [0..]) False
    

-- Public functions

toList :: Chain a -> [a]
toList = V.toList . toVector

fromList :: Ord a => [a] -> Chain a
fromList l = Chain (V.fromList l) (S.fromList . map M $ zip l [0..]) False

--Can be optimized
cReverse :: Ord a => Chain a -> Chain a
cReverse (Chain ch s rev) = Chain ch s $ not rev

(!) :: Chain a -> Int -> a
(!) (Chain ch _ False) i = ch V.! i
(!) (Chain ch _ False) i = ch V.! (V.length ch - i)

(!?) :: Chain a -> Int -> Maybe a
(!?) (Chain ch _ False) i = ch V.!? i
(!?) (Chain ch _ True) i = ch V.!? (V.length ch - i) 

cLength :: Chain a -> Int
cLength = V.length . toVector

replace :: (Ord a) => Chain a -> Int -> [a] -> Chain a
replace ch i diff = fromVector $ toVector ch V.// zip [i..] diff

cEmpty :: (Ord a) => Chain a -> a -> Bool
cEmpty (Chain _ sorted) element = not $ M (element,0) `S.member` sorted

cIndex :: (Ord a) => Chain a -> a -> Maybe Int
cIndex ch@(Chain ch sorted rev) elem = if cEmpty ch elem 
                                     then Nothing
                                     else if rev 
                                        then rix ch index
                                        else index
                     where
                        index = fmap second elem'
                        elem' = S.lookupGE (M (elem, 0)) sorted
                        second (M (_,i)) = i
