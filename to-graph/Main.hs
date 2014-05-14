import Read
import Chain
import Similarity
import System.Environment

-- converts an chain-file to a graph
fileToGraphFCC file = do
    content <- readFile file
    let coords = map readFCC $ lines content
    return $ buildGraph (fromList coords)
    
fileToGraph3d file = do
    content <- readFile file
    let coords = map read3D $ lines content
    return $ buildGraph (fromList coords)
    
putGraph graph = do
    putStr $ printGraph graph

main = do
    args <- getArgs
    case args of
        ["3d", file] -> fileToGraph3d file >>= putGraph
        [file] -> fileToGraphFCC file >>= putGraph
        otherwise -> putStrLn "Error, wrong arguments"
    
