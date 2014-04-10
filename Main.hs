module Main where
import Folder
import HPModel
import System.Environment

-- TODO: lite felhantering kanske
main :: IO ()
main = do
    (input:iterations:_) <- getArgs
    let residues = createResidues input
    res <- run residues (read iterations) :: IO FCC
    printJGReadable (head res) (length res) residues

