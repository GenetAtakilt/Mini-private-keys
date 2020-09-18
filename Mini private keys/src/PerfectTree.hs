{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PerfectTree where

import Control.Monad.State

data Perfect a = Z a | S (Perfect (a, a)) deriving Show

perfect1, perfect2,perfect5 :: Perfect Int 
perfect1 = S $ S $ Z((1,2),(3,4))
perfect2 = S $ S $S $Z (((1,2),(3,4)), ( (5,6),(7,8)))
perfect5 = S (S (S (S (S (Z (((((1,2),(3,4)),((5,6),(7,8))),(((9,10),(11,12)),((13,14),(15,16)))),((((17,18),(19,20)),((21,22),(23,24))),(((25,26),(27,28)),((29,30),(31,32))))))))))
-------------------------------------------------------------------------------------------------
-- Subtask 4.2.1
-- takes a list and produce a list with the same shape with all the leafs in reverse order        
        
reverse' :: Perfect a -> Perfect a
reverse' = fun id

fun ::(a->b)-> Perfect a-> Perfect b
fun f (Z a)= Z $ f a
fun f (S p)= S $ fun f' p
    where 
        f' (a,a')=(f a', f a) 
        
-- Subtask 4.2.2
-- takes a list and an index returns an element in that index. If the tree is Z p it will return p 
-- and if the tree has more than 1 leaf it will traverse to the tree then return a value at that index 

index :: Perfect a -> Int -> Maybe a
index (Z p) n
            | n<0 = error "Invalid index"
            | otherwise = Just( p)
index (S p) n = do 
            (a,a') <- index p (div n 2)
            Just $ if even n then a else a' 

------------------------------------------------------------------------------------------------- 
-- Subtask 4.2.3
-- in these task we are asked to build a perfect tree of height h and with leafs of list [a]
-- we did two implementation of the function  
-- 1. build checks the length of the list with the height and if it not 2^h then it will return  
--    an error message else it will return a tree of height h. But these function wont work for
--    an infinite list
-- 2. build'' will use a state monad to build a perfect tree of height h. we use state to set and get 
--    a list from it. build'' will get the list from state and calls buildS with that the state and height 
--    of the tree. Then buildS using the state and the height will build a tree.         

build :: Int -> [a] -> Perfect a
build n xs = if (length xs)==(2^n) then fromListToPerfect xs  else error "short"


fromListToPerfect ::[a]-> Perfect a
fromListToPerfect [x] = Z x
fromListToPerfect xs = S (fromListToPerfect (pair xs))
    where
        pair (x:y:ys) = (x,y):pair ys
        pair _=[]

build' :: Int -> Perfect ()
build' h = build'' h $ repeat ()

build'' :: Int -> [a] -> Perfect a
build'' h xs = evalState (buildS m h) xs
  where
    m = do
        zs <- get
        case zs of
            []       -> error "list too short"
            (y : ys) -> put ys >> return y

buildS ::forall a b . State [a] b -> Int -> State [a](Perfect b)
buildS m 0 = do
    b <- m
    return (Z b)
buildS m h = S <$>( buildS m' (h-1))
    where 
        m' ::State [a] (b,b)
        m' = do
            b <- m
            b'<- m
            return (b,b')
           