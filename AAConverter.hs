
import System.Environment
import Control.Monad
import Data.Char (toUpper)

type AA = String
type HP = String

polar :: [(AA, HP)]
polar = [ ("D","P")
        , ("E","P")
        , ("H","P")
        , ("K","P")
        , ("N","P")
        , ("O","P")
        , ("Q","P")
        , ("R","P")
        , ("S","P")
        , ("W","P")
        ]

hydrophobic :: [(AA, HP)]
hydrophobic = [ ("Y","H")
              , ("A","H")
              , ("C","H")
              , ("F","H")
              , ("G","H")
              , ("I","H")
              , ("L","H")
              , ("M","H")
              , ("P","H")
              , ("V","H")
              ]

translateAAtoHP :: [AA] -> [(AA, Maybe HP)]
translateAAtoHP = map (\x -> (x, lookup x aaTable))
  where
    aaTable = polar ++ hydrophobic

translate :: [AA] -> [HP]
translate = f . translateAAtoHP
  where
    f ((aa, Nothing):_)  = error ("Could not match the amino acid \"" ++ aa ++ "\" with any entry in the dictionary.")
    f ((aa, Just x):xs) = x : f xs
    f []                = []

main :: IO ()
main = getArgs >>= putStrLn . unwords . translate . map (map toUpper) . concatMap (concatMap (words) . lines)

