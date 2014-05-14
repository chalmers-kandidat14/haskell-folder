module Main where
import Folder
import HPModel
import GraphConverter
import Similarity
import Chain
import Print (chainToJGList) 
import System.IO
import Control.Monad (forM_)
import System.Environment
import System.Directory (createDirectoryIfMissing)

runLarge input iter repeats lattice= do
    let residues = createResidues input
    let iterations = read iter :: Int
    let n = read repeats :: Int
    createDirectoryIfMissing True "output/graphs"
    createDirectoryIfMissing True "output/chains"
    createDirectoryIfMissing True "output/energyhist"
    energyHandle <- openFile "output/energies.csv" WriteMode
    let writeOut i (res, ens) = do
                        writeFile ("output/chains/chain-" ++ show i ++ ".csv")
                                  (unlines $ map show $ toList (head res))
                        writeFile ("output/graphs/graph-" ++ show i ++ ".csv")
                                  (printGraph $ buildGraph (head res))
                        hPutStrLn energyHandle 
                                  (show $ energyWithList residues $ head res)
                        writeFile ("output/energyhist/energies" ++ show i ++ ".csv")
                                  (unlines $ map show ens)
                        putStrLn $ "Finished number " ++ show i ++ " of " ++ show n
    writeFile "output/residues.csv" (unlines $ map show residues)
    writeFile "output/iterations" $ show iter
    writeFile "output/lattice" $ show lattice
    let runfunc i = case lattice of
			"2d" -> (run residues iterations :: IO (C2d, [Double])) >>= writeOut i
			"3d" -> (run residues iterations :: IO (C3d, [Double])) >>= writeOut i
			"fcc" -> (run residues iterations :: IO (FCC, [Double])) >>= writeOut i
    forM_ [1..n] runfunc
    hClose energyHandle
    putStrLn "Saved results in folder 'output'"

printHelp = do
    putStrLn "Usage: pfolder [-l latticetype] <residues> <iterations>"
    putStrLn ""
    putStrLn "Lattice types: 2d, 2d-r, 3d, fcc (default: fcc)"
    putStrLn ""
    putStrLn "Large run: pfolder large <latticetype> <residues> <iterations> <number of runs>"


main :: IO ()
main = do
    
    args <- getArgs
    case args of
        ["convert-graph", file] -> convertAndPrintFile file
        ["large", lattice, input, iter, repeats] -> runLarge input iter repeats lattice
        ["-h"] -> printHelp
        [] -> printHelp
        _ -> putStrLn "Error: Invalid arguments" >> printHelp
        
