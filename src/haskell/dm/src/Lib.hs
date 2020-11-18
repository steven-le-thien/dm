module Lib
    ( distMat, dm, newick, randomized_dm
    ) where

import Data.List 
import Data.Map hiding (take, drop)
import Control.Monad
import System.Random

eps = 0.0001

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
                        then (fmap (+ 1) (m:ms)) ++ [0]
                        else if x - min_xs < -eps
                        then [0]
                    else (fmap (+ 1) (m:ms))

distMat :: Int -> IO([[Double]],[String])
distMat 0 = return ([],[])
distMat x = do
    l <- getLine
    let (name:ws) = words l
    let v = fmap read ws
    (next_v,next_name) <- distMat (x - 1)
    return ((v:next_v),(name:next_name))

dm_vec :: [[Double]] -> Int -> Int -> Int -> Double
dm_vec d n x y = ((d !! x) !! (n-1)) + ((d !!y) !! (n-1)) - ((d !! x) !! y)

ind_all :: [Int] -> [b] -> [b]
ind_all [] _ = []
ind_all (x:xs) a = ((a !! x):(ind_all xs a))

dm :: [[Double]] -> Int -> [Int] -> Tree 
dm _ _ [l]          = Leaf l
dm d n (l:ls)       = Node t1 t2
                        where 
                            l1 = ind_all (argmin (fmap (dm_vec d n l) ls)) ls
                            l2 = ((l:ls) Data.List.\\ l1)

                            t1 = dm d n l1
                            t2 = dm d n l2

newick :: [String] -> Int -> Tree -> String
newick name_map n t = let ('(':str) = (newick' t) in "(" ++ (name_map !! (n - 1)) ++ "," ++ str ++ ";"
    where
        newick' :: Tree->String
        newick' (Leaf l)        = name_map !! l
        newick' (Node l r)    = "(" ++ (newick' l) ++ "," ++ (newick' r) ++ ")"

shuffle :: [Int] -> IO [Int]
shuffle [] = return []
shuffle (x:xs) = getStdRandom (uniformR (0, (length (x:xs)) - 1)) >>= (\a -> return (swap 0 a (x:xs)))

--stole from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices
swap :: Int -> Int -> [Int] -> [Int]
swap a b list   | a == b    = list
                |otherwise  = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list

-- first arg sorted from smaller depth -> larger
join_subtrees :: [IO Tree] -> IO Tree
join_subtrees [] = return Error
join_subtrees [t] = t
join_subtrees (t:tt) = (join_subtrees tt) >>= (\tt' -> (t >>= (\t' -> return (Node t' tt'))))

randomized_dm :: [[Double]] -> Int -> IO [Int] -> IO Tree
randomized_dm d n llio = do
    (l:ls) <- llio
    let cod = fmap (dm_vec d n l) ls
        dom = fmap (\x -> [x]) ls
        dm_map_sum = toAscList ( fromListWith (++) (zip cod dom))
        dm_preimage = (Data.List.map (return . snd) dm_map_sum) ++ [return [l]]
        thresh = (floor . logBase 2 . fromIntegral) (length (l:ls)) 

        result 
            | length ls == 0                = return (Leaf l)
            | length dm_preimage >= thresh  = join_subtrees (fmap (randomized_dm d n) dm_preimage)
            | otherwise                     = randomized_dm d n (shuffle (l:ls))
    result 






