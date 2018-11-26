{-
Zhuravlev Roman 5303

Functional programming 

Lab 1
-}


module HomeTask where

import Data.List
import Data.Ord
import Data.Eq
import Data.Char

--Task 1

id_ :: a -> a
id_ = \ a -> a

eval :: (a -> b, a) -> b
eval (a,b) = a b

exchange :: (a, b) -> (b, a)
exchange (a,b) = (b,a)

compose :: (b -> c) -> (a -> b) -> a -> c
compose x y = x.y

curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ k = (\z x y -> z (x, y)) k

associate :: (a, (b, c)) -> ((a, b), c)
associate (a, (b ,c)) = ((a, b), c)

--Task 2

minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax lst = let  
            helper minCurr maxCurr [] = Just (minCurr, maxCurr)
            helper minCurr maxCurr (a : lstA) = helper (min minCurr a) (max maxCurr a) lstA
        in helper (head lst) (head lst) (tail lst)

--Task 3

sumQuantity :: Integer -> (Integer, Integer)
sumQuantity 0 = (0,1)
sumQuantity x  = let
        currModTen c = c `mod` 10
        helper (sumCurr, lenCurr) curr 
            |curr == 0 = (sumCurr, lenCurr)
            |curr > 0 =  helper (sumCurr + (currModTen curr), lenCurr + 1) ((curr - (currModTen curr)) `div` 10)
    in helper (0,0) (abs x)      
         
--Task 4

findCandidate :: Eq a => [a] -> a
findCandidate as = let
    helper candidate count []  = candidate
    helper candidate 0 (x:xs)  = helper x 1 xs
    helper candidate count (x:xs)
        | candidate == x = helper candidate (count + 1) xs
        | candidate /= x = helper candidate (count - 1) xs
    in helper (head as) 0 as

isMajority :: Eq a => [a] -> a -> Maybe(a)
isMajority as candidate = let
    helper count [] = if count > ((length as) `div` 2) then Just(candidate) else Nothing
    helper count (x:xs)
        | candidate == x = helper (count + 1) xs
        | candidate /= x = helper count xs
    in helper 0 as

findMajority :: Eq a => [a] -> Maybe(a)
findMajority as = isMajority as (findCandidate as)


--Task 5

f:: (a -> a) -> Int -> (a -> a)
f g n b 
    |n <= 0 = error "n must be positive number"
    |otherwise = let 
        helper acc c
            | c == 0 = acc
            | c >  0 = helper (g acc) (c - 1)
    in helper b n


-- Task 6

lastFibonacciDigit :: Int -> Int
lastFibonacciDigit n = let 
    helper fir sec c 
        | c == 0 = fir
        | c >  0 = helper (mod (fir + sec) 10) fir (c - 1)
    in helper 0 1 n

-- Task 7

isPalindrome :: String -> Bool
isPalindrome xs = let
        revList :: [a] -> [a]
        revList [] = []
        revList (x:xs)= (revList xs) ++ [x]
        helper [] [] = True
        helper [] [_] = False
        helper [_] [] = False
        helper (f:fir) (s:sec)  
            |f == s = helper fir sec
            |otherwise = False
    in helper (map toLower xs) (revList (map toLower xs))