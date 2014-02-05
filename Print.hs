import Data.List

data Coord = Coord {xCoord :: Int, yCoord :: Int} deriving (Eq)

instance Show Coord where
    show c = show (xCoord c) ++ "." ++ show (yCoord c)

type Chain = [Coord]

prog = putStrLn $ makeGrid myChain

myChain :: Chain
myChain = [Coord 0 0, Coord 0 1, Coord 1 1, Coord 1 2, Coord 1 3, Coord 2 3, Coord 2 2]

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
