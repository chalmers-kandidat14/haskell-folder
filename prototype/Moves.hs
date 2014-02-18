module Moves where 

import Chain
import Coord
import Examples



data Direction = Up | Down deriving (Eq)

data Move a = Move { before :: Chain a
                   , after :: Chain a
                   , id :: String
                   } deriving (Eq)

instance Show (Move a) where
    show (Move _ _ id) = show id

doMove :: PullMove a -> Chain a
doMove (PullMove ch i Up move) = undefined
doMove (PullMove ch i Down move) = undefined

pullMoves :: Chain -> [Move]
pullMoves = undefined

generatePullMoves' :: Chain -> Int -> Direction -> [Move]
generatePullMoves' ch i Up = let ch' = reverse ch
                            i'   = (length ch) - i - 1
                            diffs = map (pull ch' i') (nearbyPointPairs ch' i')
                          in map (Move ch i Up) diffs
generatePullMoves' ch i Down = let diffs = map (pull ch i) (nearbyPointPairs ch i)
                          in map (Move ch i Down) diffs

pull :: Coord a => Chain a -> Int -> (a, a) -> [a]
pull ch i (c, l) | ch!(i-1) == c = [l]
                 | otherwise     = l:c:(follow (i-2))
      where 
        follow 0 = [ ch ! 2 ]
        follow j | j < 0 = []
                 | ( ch!(j+2) ) `adj` ( ch!(j-1) ) = ch!(j+2) : []
                 | otherwise = ch!(j+2) : (follow (j-1))

nearbyPointsPairs :: Coord a => Chain a -> Int -> [(a, a)]
nearbyPointsPairs ch i = filter ((cEmpty ch) . snd) $ -- L must be empty
                         filter (valid . fst) $       -- C must be empty or contain i-1
                         neighbors (ch!i) (ch!(i+1)) 
    where valid coord = cEmpty ch coord || coord == (ch!(i-1))
