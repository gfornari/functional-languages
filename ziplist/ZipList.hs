{-# OPTIONS_GHC -Wall #-}

module ZipList where

import Control.Applicative hiding (ZipList)

newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
    fmap f (Z xs) = Z (map f xs)

instance Applicative ZipList where
    pure x = Z (repeat x)
    Z fs <*> Z xs = Z (zipWith ($) fs xs) -- we can use id istead of ($)

{- Assignment: write an Haskell program that, given three lists L1, l2
and L3 of length n >0, computes the list containing all lists of three
elements where the first element comes from L1, the second element from
L2 and the third from L3. -}
zipA :: [a] -> [a] -> [a] -> [[a]]
zipA xs ys zs = [ [x,y,z] | x <- xs, y <- ys, z <- zs ]

{- Assignment: Write an Haskell program that, given lists L1, L2 and L3
of length n>0, computes the list containing n lists of three elements
where for the i-th such list, the first element is the i-th element of
L1, the second element is the i-th element of L2 and the third is the
i-th element of L3. -}
zipB :: [a] -> [a] -> [a] -> [[a]]
zipB (x:xs) (y:ys) (z:zs) = [x,y,z] : zipB xs ys zs
zipB _ _ _ = []

{- Assignment: write an Haskell program that, given a list L of lists of
Int, produces a new list that is obtained from L by adding 1 to the first
list contained in L, then adding 2 to the second list of L ad so on. -}
zipC :: [[Int]] -> [[Int]]
zipC = zipWith id (zipWith id (repeat map) (map (+) [1..]))
