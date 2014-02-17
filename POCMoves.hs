module POCMoves where

import Print
import Data.Array

data Direction = Up | Down deriving (Eq)
type Move = [Coord]


revInRng :: (Int, Int) -> Int -> Int
revInRng (a, b) i = a + b - i

revChain :: AChain -> AChain
revChain ch = ixmap (bounds ch) (revInRng (bounds ch)) ch

doMove :: AChain -> Int -> Direction -> Move -> Chain
doMove ch i Up move = reverse $ doMove (revChain ch) (revInRng (bounds ch) i) Down move
doMove ch i Down move = let ch' = elems ch in take (i - length move) ch' ++ reverse move ++ drop i ch'

--allPulls :: Chain -> [Chain]
--allPulls ch = concat $ map (\

pullMoves :: AChain -> Int -> Direction -> [Move]
pullMoves ch i dir | not iValid = []
		   | dir == Up = pullMoves (revChain ch) (revInRng (bounds ch) i) Down
		   | dir == Down = map (pull ch i) (valCLs ch i)
	where
		iValid = let (a,b) = bounds ch in i>a && i<b

pull :: AChain -> Int -> (Coord, Coord) -> Move
pull ch i (c, l) | ch!(i-1) == c = [l]
		 | otherwise = l:c:(follow (i-2))
	where
		(low, _) = bounds ch
		follow j | j == low - 1 = []
			 | j == low = [ch!(j+2)]
			 | (ch!(j+2)) `adj` (ch!(j-1)) = [ch!(j+2)] 
	  		 | otherwise = ch!(j+2):(follow (j-1))

valCLs :: AChain -> Int -> [(Coord, Coord)]
valCLs ch i = filter (empty ch . snd) $ filter (valC . fst) $ neighbors (ch!i) (ch!(i+1))
	where
		valC c = c == (ch!(i-1)) || empty ch c
