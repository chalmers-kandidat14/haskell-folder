module Examples where

import Coord
import Chain

myChain :: Chain Coord2d
myChain = fromList [Coord2d 1 1, Coord2d 1 2, Coord2d 2 2, Coord2d 3 2, 
                    Coord2d 3 1, Coord2d 4 1, Coord2d 4 2, Coord2d 4 3,
                    Coord2d 4 4, Coord2d 5 4, Coord2d 6 4, Coord2d 6 3]
