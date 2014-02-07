module Print where

import Data.List
import Data.Array

data Coord = Coord {xCoord :: Int, yCoord :: Int} deriving (Eq)

instance Show Coord where
    show c = show (xCoord c) ++ "." ++ show (yCoord c)

type Chain = [Coord]

type AChain = Array Int Coord

adj :: Coord -> Coord -> Bool
adj a b = abs (xCoord a - xCoord b) + abs (xCoord a - yCoord b) == 1

dadj :: Coord -> Coord -> Bool
dadj a b =  abs (xCoord a - xCoord b) == 1 &&  abs (xCoord a - yCoord b) == 1

neighbors :: Coord -> Coord -> [(Coord, Coord)]
neighbors (Coord ax ay) (Coord bx by) | ax == bx = [(Coord (ax+1) ay, Coord (bx+1) by),(Coord (ax-1) ay, Coord (bx-1) by)]
				      | ay == by = [(Coord ax (ay+1), Coord bx (by+1)),(Coord ax (ay-1), Coord bx (by-1))]

empty :: AChain -> Coord -> Bool
empty ch c = not $ elem c (elems ch)

aChain :: Chain -> AChain
aChain ch = listArray (1,length ch) ch

deaChain :: AChain -> Chain
deaChain ch = elems ch 

prog = putStrLn $ makeGrid myChain

myChain :: Chain
myChain = [Coord 0 0, Coord 0 1, Coord 0 2, Coord 0 3, Coord 1 3, Coord 1 2, Coord 2 2]

makeGrid :: Chain -> String
makeGrid ch = unlines $ map (makeRow ch) [0..(maxY ch)]

makeRow :: Chain -> Int -> String
makeRow ch j = map mark [0..(maxX ch)]
	where
		mark i = case  elemIndex (Coord i j) ch of
				Nothing -> ' '
				Just n -> head $ show n

maxX :: Chain -> Int
maxX cs = foldl max 0 (map xCoord cs)

maxY :: Chain -> Int
maxY cs = foldl max 0 (map yCoord cs)
