module Main where
import Folder
import HPModel
import System.Environment

printHelp = do
    putStrLn "Usage: pfolder [-l latticetype] residues iterations"
    putStrLn "       Lattice types: 2d, 2d-r, 3d, fcc (default: fcc)"

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

main :: IO ()
main = do
    
    args <- getArgs
    case args of
        [input, iterations] -> runFCC input iterations 
        ["-l", "fcc", input, iterations] -> runFCC input iterations       
        ["-l", "2d", input, iterations] -> run2d input iterations
        ["-l", "2d-r", input, iterations] -> run2dReadable input iterations 
        ["-l", "3d", input, iterations] -> run3d input iterations
        ["-h"] -> printHelp
        [] -> printHelp
        _ -> putStrLn "Error: Invalid arguments" >> printHelp
        
