import System.Random

generateList :: IO [Int]
generateList = sequence (replicate 24 (randomRIO (0,99 :: Int)))
