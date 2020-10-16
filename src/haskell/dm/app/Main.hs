module Main where

import Lib

main :: IO()
main = do
    n <- readLn
    (d,name_map) <- distMat n
    t <- dm d n [0..(n-2)]
    putStrLn (newick name_map n t)
    --putStrLn (newick name_map n (dm d n [0..(n-2)]))
