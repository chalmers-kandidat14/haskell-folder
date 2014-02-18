module Print (printChain) where

import Chain
import Coord
import Data.List (elemIndex)

printChain :: Chain Coord2d -> IO ()
printChain = putStr . showRows

showRows :: Chain Coord2d -> String
showRows chain = unlines $ map (showRow coords) [yMin..yMax]
    where
        coords = toList chain
        yMin = minimum . map yCoord $ coords
        yMax = maximum . map yCoord $ coords

showRow ::  [Coord2d] -> Int -> String
showRow chain j = 
    foldr print "" gridrow
    where
        gridrow = [Coord2d x j | x <- [xMin..xMax]]
        xMin = minimum . map xCoord $ chain
        xMax = maximum . map xCoord $ chain
        addSpace str =  if length str < 3
                        then addSpace (' ':str)
                        else str
        print cell output = case elemIndex cell chain of
                                Nothing -> "   " ++ output
                                Just i -> addSpace (show i) ++ output

