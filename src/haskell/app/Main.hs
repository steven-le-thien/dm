module Main where

import Lib
import Control.Arrow
import Data.List

main = interact $
  lines >>> map (words >>> map read >>> solve >>> show) >>> unlines

solve :: [Integer] -> Integer
solve [a,b] = abs (a - b)

-- Tree ADT
data Tree = Leaf Int | Node Int Tree Tree | Error deriving Show

-- DM(L, new_root, dist_orc)
-- Step 1. Check |L|
-- Step 2. Query DM vector on pivot
-- Step 3. Form subtree L1, L2
-- Step 4. Recurse
-- Step 5. Join tree

argmin :: [Int] -> [Int]
argmin [] = []
argmin [x] = [0]
argmin (x:xs) = let     (m:ms) = argmin xs
                        min_xs = xs !! m
                in
                    if x == min_xs
                        then (map (+ 1) (m:ms)) ++ [0]
                    else if x < min_xs
                            then [0]
                    else (map (+ 1) (m:ms))

dist :: Int -> Int -> Int
dist 2 1 = 15
dist 1 2 = 15
dist 0 2 = 31
dist 2 0 = 31
dist 2 3 = 26
dist 3 2 = 26
dist 1 0 = 32
dist 0 1 = 32
dist 1 3 = 27
dist 3 1 = 27
dist 0 3 = 25
dist 3 0 = 25
dist 0 4 = 26
dist 4 0 = 26
dist 1 4 = 18
dist 4 1 = 18
dist 2 4 = 17
dist 4 2 = 17
dist 3 4 = 21
dist 4 3 = 21
dist _ _ = 0

dm_vec :: Int -> Int -> Int
dm_vec x y = (dist x 4) + (dist y 4) - (dist x y)

search :: [Int] -> [Int] -> [Int]
search js xs = [xs !! j | j <- js]

dm :: [Int] -> Tree
dm [l]          = Leaf l
dm [l,r]        = Node 0 (Leaf l) (Leaf r)
dm (l:ls)       = Node 0 t1 t2
                        where
                            --t1 = case dm l1 of  Leaf l -> Leaf l
                            --                    Node _ t _ -> t
                            --t2 = case dm l2 of  Leaf l -> Leaf l
                            --                   Node _ t _ -> t
                            t1 = dm l1
                            t2 = dm l2

                            l1 = map (+ 1) (argmin (map (dm_vec l) ls))
                            l2 = ((l:ls) \\ l1) 
