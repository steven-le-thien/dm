module Main where

import Lib

main :: IO()
main = do
    n <- readLn
    (d,name_map) <- distMat n
    putStrLn (newick name_map n (randomized_dm d n [0..(n-2)]))
        --where t = dm d n [0..(n - 2)]
    --putStrLn (newick name_map n (dm d n [0..(n-2)]))
