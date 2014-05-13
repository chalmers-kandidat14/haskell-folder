import Read
import Chain
import Similarity
import System.Environment

-- converts an chain-file to a graph
fileToGraph file = do
    content <- readFile file
    let coords = map readFCC $ lines content
    return $ buildGraph (fromList coords)
    
main = do
    (file:_) <- getArgs
    graph <- fileToGraph file
    putStr $ printGraph graph
    
