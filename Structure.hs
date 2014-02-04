
import Control.Monad (mapM_)

data Coord = Coord { xCoord :: Int, yCoord :: Int } deriving (Eq)

instance Show Coord where
   show (Coord x y) = (show x) ++ "x" ++ (show y)

type Chain = [Coord]

showRow :: Chain -> Int -> String
showRow chain j = 
    foldr print "" gridrow
    where
        gridrow = [Coord x j | x <- [1..xMax]]
        xMax = maximum . map xCoord $ chain
        print cell output
            | cell `elem` chain = 'X':output
            | otherwise          = ' ':output


showRows :: [Coord] -> [String]
showRows coords = map (showRow coords) [1..yMax]
    where
        yMax = maximum . map yCoord $ coords

myChain :: Chain
myChain = (Coord 1 1) : (Coord 1 2) : (Coord 2 2) : (Coord 3 2) : 
          (Coord 3 1) : (Coord 4 1) : (Coord 4 2) : (Coord 4 3) :
          (Coord 4 4) : []


main :: IO ()
main = mapM_ putStrLn (showRows myChain) 
