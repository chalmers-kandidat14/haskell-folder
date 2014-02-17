module PullMoves where

import Chain

data Direction = Up | Down deriving (Eq)
data Move = Move { getChain :: Chain
                 , getIndex :: Int
                 , direction :: Direction
                 , difference :: [Coord]
                 } deriving (Eq)

instance Show Move where
    show (Move ch i Up _) = (show (ch!!i)) ++ " up"
    show (Move ch i Down _) = (show (ch!!i)) ++ " down"

doMove :: Move -> Chain
doMove (Move _ _ Up _) = undefined
doMove (Move ch i Down move) = take (i - length move) ch ++
                               reverse move ++
                               drop i ch

generateMoves :: Chain -> Int -> Direction -> [Move]
generateMoves ch i Up = undefined
generateMoves ch i Down = let diffs = map (pull ch i) (nearbyPointPairs ch i)
                          in map (Move ch i Down) diffs

pull :: Chain -> Int -> (Coord, Coord) -> [Coord]
pull ch i (c, l) | ch !! (i-1) == c = [l]
                 | otherwise        = l:c:(follow (i-2))
      where 
        follow 0 = [ ch !! 2 ]
        follow j | (ch !! (j+2)) `adj` (ch !! (j-1)) = [ch !! (j+2)]
                 | otherwise = ch!!(j+2):(follow (j-1))
        adj a b  = abs (xCoord a - xCoord b) + abs (yCoord a - yCoord b) == 1

-- Gives a list of valid points that is called C and L in the paper
nearbyPointPairs :: Chain -> Int -> [(Coord, Coord)]
nearbyPointPairs ch i = filter (empty . snd) $ -- L must be empty
                        filter (valid . fst) $ -- C must have i-1 or be empty 
                        neighbors (ch !! i) (ch !! (i+1))
    where 
        valid coord = (ch !! (i-1)) == coord || empty coord
        empty coord = not (coord `elem` ch)
        neighbors (Coord ax ay) (Coord bx by)
                | ax == bx = [ (Coord (ax+1) ay, Coord (bx+1) by),
                               (Coord (ax-1) ay, Coord (bx-1) by) ]
                | ay == by = [ (Coord ax (ay+1), Coord bx (by+1)),
                               (Coord ax (ay-1), Coord bx (by-1)) ]


