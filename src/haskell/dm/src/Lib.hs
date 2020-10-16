module Lib
    ( distMat, dm, newick
    ) where

import Data.List
import Control.Monad

-- Tree ADT
data Tree = Leaf Int | Node Int Tree Tree | Error deriving Show

intNode = -1

argmin :: [Int] -> [Int]
argmin [] = []
argmin [x] = [0]
argmin (x:xs) = let
                    (m:ms) = argmin xs
                    min_xs = xs !! m
                in
                    if x == min_xs
                        then (map (+ 1) (m:ms)) ++ [0]
                    else if x < min_xs
                        then [0]
                    else (map (+ 1) (m:ms))

distMat :: Int -> IO[[Int]]
distMat 0 = return []
distMat x = do
    str <- getLine
    nextInt <- distMat (x - 1)
    let int = map read $ words str :: [Int]
    return (int:nextInt)

dm_vec :: [[Int]] -> Int -> Int -> Int -> Int
dm_vec d n x y = ((d !! x) !! (n-1)) + ((d !!y) !! (n-1)) - ((d !! x) !! y)

dm :: [[Int]] -> Int -> [Int] -> Tree
dm _ _ [l]          = Leaf l
dm _ _ [l,r]        = Node intNode (Leaf l) (Leaf r)
dm d n (l:ls)       = Node intNode t1 t2
                        where
                            t1 = dm d n l1
                            t2 = dm d n l2

                            l1 = map (+ 1) (argmin (map (dm_vec d n l) ls))
                            l2 = ((l:ls) \\ l1)

newick :: Int -> Tree -> String
newick n t = let ('(':str) = (newick' t) in "(" ++ (show (n - 1)) ++ "," ++ str ++ ";"
    where
        newick' :: Tree->String        
        newick' (Leaf l)        = show l
        newick' (Node _ l r)    = "(" ++ (newick' l) ++ "," ++ (newick' r) ++ ")"

