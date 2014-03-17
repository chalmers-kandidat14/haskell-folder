{-#LANGUAGE FlexibleInstances#-}
module Print (
               printChain
             , printHP
             , printSimilar
             , chainToJGList
             ) where

import Chain
import Coord
import Moves
import HPModel
import qualified Data.Vector as V
import Data.List (elemIndex)

chainToJGList :: Chain Coord2d -> [HPResidue] -> [String]
chainToJGList ch hpl = zipWith format (toList ch) hpl
	where
		format coord hp = x coord ++ " " ++ y coord ++ " 0 " ++ show hp
		x = show . xCoord
		y = show . yCoord

printChain :: Chain Coord2d -> IO ()
printChain = putStr . showRows

printHP :: V.Vector HPResidue -> Chain Coord2d -> IO ()
printHP res ch = putStr $ showRowsWith disp ch
    where disp = show . (res V.!)

printSimilar :: Chain Coord2d -> IO ()
printSimilar ch = mapM_ f (pullMoves ch)
    where f m = putStrLn "------" >> (printChain . after $ m)

showRowsWith :: (Int -> String) -> Chain Coord2d -> String
showRowsWith disp chain = unlines $ map (showRowWith disp coords) [yMin..yMax]
    where
        coords = toList chain
        yMin = minimum . map yCoord $ coords
        yMax = maximum . map yCoord $ coords

showRows :: Chain Coord2d -> String
showRows = showRowsWith show 

showRowWith ::  (Int -> String) -> [Coord2d] -> Int -> String
showRowWith disp chain j = 
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
                                Just i -> addSpace (disp i) ++ output

instance Show (Chain Coord2d) where
        show = showRows

