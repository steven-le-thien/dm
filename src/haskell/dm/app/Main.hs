module Main where

import Lib

main :: IO()
main = do
    n <- readLn
    d <- distMat n
    putStrLn (newick n (dm d n [0..(n-2)]))
