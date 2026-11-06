-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- ex 1: countnothings with a fold. the function countnothings from
-- the course material can be implemented using foldr. your task is to
-- define counthelper so that the following definition of countnothings
-- works.
--
-- hint: you can start by trying to add a type signature for counthelper.
--
-- challenge: look up the maybe function and use it in counthelper.
--
-- examples:
--   countnothings []  ==>  0
--   countnothings [just 1, nothing, just 3, nothing]  ==>  2

countnothings :: [Maybe a] -> Int
countnothings xs = foldr countHelper 0 xs

countHelper :: Maybe a -> Int -> Int
countHelper (Just a) b = b
countHelper Nothing b = b + 1

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

maxHelper :: Int -> Int -> Int
maxHelper x y = if x > y then x else y

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
-- sumAndLength []             ==>  (0.0,0)


sumAndLength :: [Double] -> (Double,Int)
sumAndLength [] = (0.0,0)
sumAndLength xs = foldr slHelper slStart xs

slStart :: (Double, Int)
slStart = (0.0,0)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (a,b) = ((a + x), (b + 1)) 

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs


concatStart :: [a]
concatStart = []

concatHelper :: [a] -> [a] -> [a]
concatHelper xs ys = xs ++ ys

-----------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]
largestHelper x xs = 
    
    if head xs < x 
    then [x] 
    else if head xs == x
    then x : xs
    else xs
     


------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myhead []  ==>  nothing
--   myhead [1,2,3]  ==>  just 1

myhead :: [a] -> Maybe a
myhead xs = foldr headHelper Nothing xs

headHelper :: a -> Maybe a -> Maybe a
headHelper x _ = Just x

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

lastHelper :: a -> Maybe a -> Maybe a
lastHelper a Nothing = Just a
lastHelper _ (Just b) = Just b





