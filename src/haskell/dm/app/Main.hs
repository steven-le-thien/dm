module Main where

import Lib
import System.CPUTime

main :: IO()
main = do
    n <- readLn
    (d,name_map) <- distMat n
    start <- getCPUTime
    putStrLn (newick name_map n (dm d n [0..(n-2)]))

    --t <- randomized_dm d n (return [0..(n-2)])
    --putStrLn (newick name_map n t)

    end <- getCPUTime
    putStrLn (show (end - start))
        --where t = dm d n [0..(n - 2)]
    --putStrLn (newick name_map n (dm d n [0..(n-2)]))
