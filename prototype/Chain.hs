module Chain where

class (Eq a, Ord a) => Coord a where
    adj :: a -> a -> Bool
    dadj :: a -> a -> Bool
    neighbors :: a -> a -> [(a,a)]


class Chain a where
    cReverse :: a -> a
    (!) :: Coord b => Int -> a -> b
    cHead ::  Coord b => a -> b
    --cTake :: a -> a
    --cDrop :: a -> a
    cTail :: a -> a

data Coord2d = Coord2d (Int, Int) deriving (Ord, Eq)

instance Coord Coord2d where
    adj _ _ = True
    dadj _ _ = True
    neighbors a b = [(a,b)]

data Coord a => LChain a = LChain {
    toList :: [a]
    }

instance Coord a => Chain (LChain a) where
    cReverse = LChain . reverse . toList
    (!) el = (el!!) . toList
