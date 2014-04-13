module Main where
import Folder
import HPModel
import Similarity
import Print (chainToJGList)
import Control.Monad (forM_)
import System.Environment
import System.Directory (createDirectoryIfMissing)

runLarge = do
    let residues = createResidues "HPH2P2H4PH3P2H2P2HPH3PHPH2P2H2P3HP8H2"
    let n = 1000
    let iterations = 10000
    let writeOut i res = do
                        writeFile ("output/chain-" ++ show i ++ ".txt")
                                  (unlines $ chainToJGList (head res) residues)
                        writeFile ("output/graph-" ++ show i ++ ".txt")
                                  (printGraph $ buildGraph residues (head res))
                        putStrLn $ "Finished number " ++ show i ++ " of " ++ show n
    
    createDirectoryIfMissing True "output"
    forM_ [1..n] (\i -> (run residues iterations :: IO FCC) >>= (writeOut i))

run2d input iterations  = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO C2d
    printJGReadable (head res) (length res) residues
        
run2dReadable input iterations  = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO C2d
    printHReadable (head res) (length res) residues

runFCC input iterations = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO FCC
    printJGReadable (head res) (length res) residues

run3d input iterations  = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO C3d
    printJGReadable (head res) (length res) residues 

printHelp = do
    putStrLn "Usage: pfolder [-l latticetype] residues iterations"
    putStrLn "       Lattice types: 2d, 2d-r, 3d, fcc (default: fcc)"


main :: IO ()
main = do
    
    args <- getArgs
    case args of
        [input, iterations] -> runFCC input iterations 
        ["-l", "fcc", input, iterations] -> runFCC input iterations       
        ["-l", "2d", input, iterations] -> run2d input iterations
        ["-l", "2d-r", input, iterations] -> run2dReadable input iterations 
        ["-l", "3d", input, iterations] -> run3d input iterations
        ["large"] -> runLarge
        ["-h"] -> printHelp
        [] -> printHelp
        _ -> putStrLn "Error: Invalid arguments" >> printHelp
        
