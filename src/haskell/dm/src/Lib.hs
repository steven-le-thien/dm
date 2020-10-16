module Lib
    ( distMat, dm, newick
    ) where

import Data.List
import Control.Monad

-- Tree ADT
data Tree = Leaf Int | Node Tree Tree | Error deriving Show

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

distMat :: Int -> IO([[Int]],[String])
distMat 0 = return ([],[])
distMat x = do
    str <- getLine
    (next_v,next_name) <- distMat (x - 1)
    let (name:v) = map read $ words str :: [Int]
    return ((v:next_v),((show name):next_name))

dm_vec :: [[Int]] -> Int -> Int -> Int -> Int
dm_vec d n x y = ((d !! x) !! (n-1)) + ((d !!y) !! (n-1)) - ((d !! x) !! y)

ind_all :: [Int] -> [Int] -> [Int]
ind_all [] _ = []
ind_all (x:xs) a = ((a !! x):(ind_all xs a))

dm :: [[Int]] -> Int -> [Int] -> IO Tree
dm _ _ [l]          = return (Leaf l)
dm _ _ [l,r]        = return (Node (Leaf l) (Leaf r))
dm d n (l:ls)       = do
                        t1 <- dm d n l1
                        t2 <- dm d n l2
                        return (Node t1 t2)
                            where
                            --t1 <- dm d n l1
                            --t2 <- dm d n l2

                            l1 = ind_all (argmin (map (dm_vec d n l) ls)) ls
                            l2 = ((l:ls) \\ l1)

newick :: [String] -> Int -> Tree -> String
newick name_map n t = let ('(':str) = (newick' t) in "(" ++ (name_map !! (n - 1)) ++ "," ++ str ++ ";"
    where
        newick' :: Tree->String
        newick' (Leaf l)        = name_map !! l
        newick' (Node l r)    = "(" ++ (newick' l) ++ "," ++ (newick' r) ++ ")"

