module Lib
    ( distMat, dm, newick
    ) where

import Data.List
import Control.Monad

eps = 1

-- Tree ADT
data Tree = Leaf Int | Node Tree Tree | Error deriving Show

argmin :: [Double] -> [Int]
argmin [] = []
argmin [x] = [0]
argmin (x:xs) = let
                    (m:ms) = argmin xs
                    min_xs = xs !! m
                in
                    if abs(x - min_xs) < eps
                        then (map (+ 1) (m:ms)) ++ [0]
                    else if x - min_xs < -eps
                        then [0]
                    else (map (+ 1) (m:ms))

distMat :: Int -> IO([[Double]],[String])
distMat 0 = return ([],[])
distMat x = do
    l <- getLine
    let (name:ws) = words l
    let v = map read ws
    (next_v,next_name) <- distMat (x - 1)
    return ((v:next_v),(name:next_name))

dm_vec :: [[Double]] -> Int -> Int -> Int -> Double
dm_vec d n x y = ((d !! x) !! (n-1)) + ((d !!y) !! (n-1)) - ((d !! x) !! y)

ind_all :: [Int] -> [b] -> [b]
ind_all [] _ = []
ind_all (x:xs) a = ((a !! x):(ind_all xs a))

dm :: [[Double]] -> Int -> [Int] -> IO Tree
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

