module ListsContinued () where

import Shared

import Lists

import Data.List (foldl')

{-
PROBLEM 11
(*) Modify run-length encoding in such a way that if an element
has no duplicates, it is simply copied into the result list. Only 
elements with duplicates are transferred as (N E) lists.
λ> encodeModified "aaaabccaadeeee"
[Multiple 4 'a', Single 'b', Multiple 2 'c',
 Multiple 2 'a', Single 'd', Multiple 4 'e']
-}
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map encode' . pack
    where encode' x
            | length x == 1 = Single (head x)
            | otherwise     = Multiple (length x) (head x)

{-
PROBLEM 12 
(**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem 11.
Construct its uncompressed version.
λ> decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c',
        Multiple 2 'a', Single 'd', Multiple 4 'e']
"aaaabccaadeeee"
-}
decodeModified :: Eq a => [Encoding a] -> [a]
-- decodeModified :: Eq a => [Encoding a] -> [a]
decodeModified = concatMap decode'
    where
        decode' (Single x)     = [x]
        decode' (Multiple i x) = replicate i x

{-
PROBLEM 13
(**) Run-length encoding of a list (direct solution)
-}
-- encodeDirect :: Eq a => [a] -> [Encoding a]

{-
PROBLEM 14
(*) Duplicate elements of a list 
λ> dupli [1,2,3]
[1,1,2,2,3,3]
-}
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

{-
PROBLEM 15
(**) Replicate the elements of a list a given number of times
λ> repli "abc" 3
"aaabbbccc"
-}
repli :: [a] -> Int -> [a]
repli xs i = concatMap (replicate i) xs

{-
PROBLEM 16
(**) Drop every N'th element from a list
λ> dropEvery "abcdefghik" 3
"abdeghk"
-}
dropEvery :: [a] -> Int -> [a]
dropEvery xs i = map snd . filter drop' . zip [0..] $ xs
    where drop' (idx, e) = idx `mod` i /= i - 1

{-
PROBLEM 17
(*) Split a list into two parts; the length of the first part is 
given. Do not use any predefined predicates.
λ> split "abcdefghik" 3
("abc", "defghik")
-}
split :: [a] -> Int -> ([a], [a])
split xs i = (take i xs, drop i xs)

{-
PROBLEM 18
(**) Extract a slice from a list 
Given two indices, i and k, the slice is the list containing the 
elements between the i'th and k'th element of the original list.
Start counting the elements with 1.
λ> slice "abcdefhik" 3 7
"cdefg"
-}
slice :: [a] -> Int -> Int -> [a] 
slice xs i k = fst . flip split (k + 1 - i) . snd . flip split (i - 1) $ xs

{-
PROBLEM 19
(**) Rotate a list N places to the left
Hint: Use the predefined functions length and (++)
λ> rotate "abcdefgh" 3
"defghabc"

λ> rotate "abcdefgh" (-2)
ghabcdef
-}
rotate :: [a] -> Int -> [a]
rotate xs i 
    | i >= 0    = let (l1, l2) = split xs i in l2 ++ l1
    | otherwise = rotate xs (length xs - abs i)