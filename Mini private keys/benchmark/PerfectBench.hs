import Criterion.Main
-------------------------------------------------------------------------------------------------
-- Subtask 4.3 Benchmarking 
-- We use two function to find an element in index i. 
-- 1. index : using toList it will produce a list of element then return an element at that index
--    It will work fine but has low performance. PerfectTOList function has n complexity and (!!) has 
--    n complexity 
-- 2. index': these will tranverse in to the list and returns an element in that index. The problem with these 
--    implementation is it wont check if the given index is greater that the length of the tree.


-- so by running stack bench or cabal bench we can see the complexity of the two implementation for the tree of height 
-- 2, 3 and 5. and we can see that indexT(2) has better performance than indexList.      
      


main :: IO () 
main = defaultMain [ 
    bgroup "index" [
      bench "IndexWithList of height 2 : First" $ whnf index1 0
    , bench "IndexWithList of height 2 : Middle" $ whnf index1 1
    , bench "IndexWithList of height 2 : Last" $ whnf index1 3
    , bench "IndexWithList of height 3 : First" $ whnf index2 0
    , bench "IndexWithList of height 3 : Middle" $ whnf index2 4
    , bench "IndexWithList of height 3 : Last" $ whnf index2 7
    , bench "IndexWithList of height 5 : First" $ whnf index5 0
    , bench "IndexWithList of height 5 : Middle" $ whnf index5 16
    , bench "IndexWithList of height 5 : Last" $ whnf index5 31
    , bench "IndexT of height 2 : First" $ whnf index1' 0
    , bench "IndexT of height 2 : Middle" $ whnf index1' 1
    , bench "IndexT of height 2 : Last" $ whnf index1' 3
    , bench "IndexT of height 3 : First" $ whnf index2' 0
    , bench "IndexT of height 3 : Middle" $ whnf index2' 4
    , bench "IndexT of height 3 : Last" $ whnf index2' 7
    , bench "IndexT of height 5 : First" $ whnf index5' 0
    , bench "IndexT of height 5 : Middle" $ whnf index5' 16
    , bench "IndexT of height 5 : Last" $ whnf index5' 31
    ]
  ]

data Perfect a = Z a | S (Perfect (a, a)) deriving Show


perfect1, perfect2,perfect5 :: Perfect Int 
perfect1 = S $ S $ Z((1,2),(3,4))
perfect2 = S $ S $S $Z (((1,2),(3,4)), ( (5,6),(7,8)))
perfect5 = S (S (S (S (S (Z (((((1,2),(3,4)),((5,6),(7,8))),(((9,10),(11,12)),((13,14),(15,16)))),((((17,18),(19,20)),((21,22),(23,24))),(((25,26),(27,28)),((29,30),(31,32))))))))))


index1 :: Int -> Maybe Int
index1 n = indexList perfect1 n 
index2 :: Int -> Maybe Int
index2 n = indexList perfect2 n 
index5 :: Int -> Maybe Int
index5 n = indexList perfect5 n 

indexList :: Perfect a -> Int -> Maybe a
indexList a i = case (perfectToList a) of 
                []-> Nothing
                xs -> if length xs >i then Just (xs !! i) else Nothing

perfectToList :: Perfect a ->[a]
perfectToList (Z a)= [a]
perfectToList (S p)= do
    (a,a')<- perfectToList p 
    [a,a']

    
index1' :: Int -> Maybe Int
index1' n = indexT perfect1 n   
index2' :: Int -> Maybe Int
index2' n = indexT perfect2 n 
index5' :: Int -> Maybe Int
index5' n = indexT perfect5 n    
    
indexT :: Perfect a -> Int -> Maybe a
indexT (Z p) n
            | n<0 = error "Invalid index"
            | otherwise = Just( p)
indexT (S p) n = do 
            (a,a') <- indexT p (div n 2)
            Just $ if even n then a else a' 