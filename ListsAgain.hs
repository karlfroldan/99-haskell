module ListsAgain () where 

import ListsContinued
import Lists (compress, elementAt)

import System.Random
    ( randoms, newStdGen, mkStdGen )
{-
PROBLEM 21
(*) Insert an element at a given position into a list
λ> insertAt 'x' "abcd" 2 
"aXbcd"
-}
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs k = l1 ++ [c] ++ l2 
    where (l1, l2) = split xs (k - 1)

{-
PROBLEM 22
Create a list containing all integers within a given range
λ> range 4 9
-}
range :: Integral a => a -> a -> [a]
range i j = [i..j]

{-
PROBLEM 23
Extract a given number of randomly selected elements from a list
λ> rndSelect "abcdefgh" 3 >>= putStrLn
eda
-}
rndSelect :: [a] -> Int -> [a]
rndSelect xs i = elementAt' xs <$> idxs i
    where idxs = fmap (`mod` length xs) . take i . randoms . mkStdGen
          elementAt' ys j = elementAt ys (j + 1)