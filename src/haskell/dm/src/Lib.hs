module Lib
  ( distMat
  , dm
  , newick
  , randomized_dm
  ) where

import           Data.List                     as L
import           Data.Map                hiding ( take
                                                , drop
                                                )
import           Control.Monad
import           System.Random

eps = 5

-- Tree ADT
data Tree = Leaf Int | Node Tree Tree | Error deriving Show

----------------------------------------------------------------------
-- DM algorithm implementation
----------------------------------------------------------------------

-- Read input distance matrix from IO
distMat :: Int -> IO ([[Double]], [String])
distMat 0 = return ([], [])
distMat x = do
  l <- getLine
  let (name : ws) = words l
      v           = (read <$> ws)
  (next_v, next_name) <- distMat (x - 1)
  return ((v : next_v), (name : next_name))

-- Compute DM vector, given the distance matrix, the number of leaves and the
-- two leaves to compute DM from
dm_vec :: [[Double]] -> Int -> Int -> Int -> Double
dm_vec d n x y =
  (((d !! x) !! (n - 1)) + ((d !! y) !! (n - 1)) - ((d !! x) !! y))

-- Run deterministic DM algorithm on input distance matrix, number of leaves
-- and taxa labels
dm :: [[Double]] -> Int -> [Int] -> Tree
dm _ _ [l     ] = Leaf l
dm d n (l : ls) = Node t1 t2
 where
  l1 = ind_all (argmin ((dm_vec d n l) <$> ls)) ls
  l2 = ((l : ls) L.\\ l1)

  t1 = dm d n l1
  t2 = dm d n l2

-- Run randomized DM algorithm on input distance matrix, number of leaves and
-- taxa labels
randomized_dm :: [[Double]] -> Int -> IO [Int] -> IO Tree
randomized_dm d n llio =
  llio
    >>= (\(l : ls) -> case () of
          _
            | length ls == 0
            -> return (Leaf l)
            | length dm_preimage >= thresh - 2
            -> join_subtrees $ randomized_dm d n <$> dm_preimage
            | otherwise
            -> randomized_dm d n (shuffle (l : ls))
           where
            cod        = dm_vec d n l <$> ls
            dom        = return <$> ls
            dm_map_sum = toAscList $ fromListWith (++) (zip cod dom)
            dm_preimage =
              (return . snd <$> ball_cluster dm_map_sum) ++ [return [l]]
            thresh = floor $ logBase 2 $ fromIntegral $ length (l : ls)
        )

----------------------------------------------------------------------
-- Tree utilities
----------------------------------------------------------------------

-- Given a name map, the number of nodes and a tree data structure, return the
-- Newick string that represents the tree
newick :: [String] -> Int -> Tree -> String
newick name_map n t =
  let ('(' : str) = (newick' t)
  in  "(" ++ (name_map !! (n - 1)) ++ "," ++ str ++ ";"
 where
  newick' :: Tree -> String
  newick' (Leaf l  ) = name_map !! l
  newick' (Node l r) = "(" ++ (newick' l) ++ "," ++ (newick' r) ++ ")"

-- Given a list of trees (sorted with shallower trees first), return the joined
-- supertree whose backbone is a caterpillar tree
join_subtrees :: [IO Tree] -> IO Tree
join_subtrees []       = return Error
join_subtrees [t     ] = t
join_subtrees (t : tt) = ((<*>) . (Node <$>)) t (join_subtrees tt)


----------------------------------------------------------------------
-- General utility functions
----------------------------------------------------------------------

-- Given an array A and an array of index I, return A[I]
ind_all :: [Int] -> [b] -> [b]
ind_all []       _ = []
ind_all (x : xs) a = ((a !! x) : (ind_all xs a))

-- Given an array A, probe the global PRNG for a uniformly random index i > 0
-- and swap the element A[0] with A[i] 
shuffle :: [Int] -> IO [Int]
shuffle xs = swap xs 0 <$> getStdRandom (uniformR (0, (length xs) - 1))

-- Given an array A, two indices i, j (may be equal, swap A[i] and A[j])
--stole from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices
swap :: [Int] -> Int -> Int -> [Int]
swap [] _ _ = []
swap list a b
  | a == b    = list
  | otherwise = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
 where
  list1 = take a list
  list2 = drop (succ a) (take b list)
  list3 = drop (succ b) list


-- Given key-sorted map: k maps to [a] and diameter eps, if (k,a), (b, c) in
-- map such that |k - b| <= eps then merge a and c. Proceed from lowest k
-- greedily
ball_cluster :: [(Double, [Int])] -> [(Double, [Int])]
ball_cluster as = reverse $ cluster' as []
 where
  cluster' []         b  = b
  cluster' (ka : kas) [] = cluster' kas [ka]
  cluster' ((k, a) : kas) ((b, c) : bcs)
    | k - b <= eps = cluster' kas ((b, c ++ a) : bcs)
    | otherwise    = cluster' kas ((k, a) : (b, c) : bcs)


-- Given an array A of double, return all indices that minimizes A[i]
argmin :: [Double] -> [Int]
argmin []  = []
argmin [x] = [0]
argmin (x : xs) | abs (x - min_xs) < 0.1 = ((+ 1) <$> (m : ms)) ++ [0]
                | x - min_xs < -0.1      = [0]
                | otherwise              = ((+ 1) <$> (m : ms))
 where
  (m : ms) = argmin xs
  min_xs   = xs !! m


