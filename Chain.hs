

import Data.List (elemIndex)

data Coord = Coord { xCoord :: Int, yCoord :: Int } deriving (Eq)

instance Show Coord where
   show (Coord x y) = show x ++ "x" ++ show y

type Chain = [Coord]

showRow :: Chain -> Int -> String
showRow chain j = 
    foldr print "" gridrow
    where
        gridrow = [Coord x j | x <- [1..xMax]]
        xMax = maximum . map xCoord $ chain
        addSpace str =  if length str < 3
                        then addSpace (' ':str)
                        else str
        print cell output = case elemIndex cell chain of
                                Nothing -> "   " ++ output
                                Just i -> addSpace (show i) ++ output

showRows :: [Coord] -> String
showRows coords = unlines $ map (showRow coords) [1..yMax]
    where
        yMax = maximum . map yCoord $ coords

myChain :: Chain
myChain = [Coord 1 1, Coord 1 2, Coord 2 2, Coord 3 2, 
          Coord 3 1, Coord 4 1, Coord 4 2, Coord 4 3,
          Coord 4 4, Coord 5 4, Coord 6 4, Coord 6 3]


printChain :: Chain -> IO ()
printChain = putStr . showRows
