module Lists (pack) where

import Shared 
    (NestedList (..))

import Data.List (foldl')
{-
PROBLEM 1
(*) Find the last element of a list
λ> myLast [1,2,3,4]
4

λ> myLast "xyz"
'z'
-}
myLast :: [a] -> a 
myLast = foldr1 (const id)

{-
PROBLEM 2
(*) Find the lat but one element of a list
λ> myButLast [1,2,3,4]
3

λ> myButLast ['a'..'z']
'y'
-}
myButLast :: [a] -> a 
myButLast = myLast . init

{-
PROBLEM 3
(*) Find the k'th element of a list. The first element in the list 
is number 1.
λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}
elementAt :: [a] -> Int -> a 
elementAt [] _ = error "Ensure list is nonempty"
elementAt (x:_) 1 = x 
elementAt (x:xs) k = elementAt xs (k - 1)

{-
PROBLEM 4
(*) Find the element of the list 
λ> myLength [123, 456, 789]
3

λ> myLength "Hello, world!"
13
-}
myLength :: [a] -> Int 
myLength = foldr (const (+1)) 0

{-
PROBLEM 5
(*) Reverse a list
λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"

λ> myReverse [1,2,3,4]
[4,3,2,1]
-}
myReverse :: [a] -> [a] 
myReverse = foldl' (flip (:)) []

{-
PROBLEM 6
(*) Find out whether a list is a palindrome
λ> isPalindrome [1,2,3]
False

λ> isPalindrome "madaminadam"
True

λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-}
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome xs = myReverse xs == xs

{-
PROBLEM 7
(*) Flatten a nested list structure

Transform a list, possibly holding lists as elements into a 'flat' list 
by replacing each list with its elements

data NestedList a = Elem a | List [NestedList a]
λ> flatten (Elem 5)
[5]

λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-}
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = [] 
flatten (List xs) = foldl' (\acc x -> acc ++ flatten x) [] xs

{-
PROBLEM 8
(**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced
with a single copy of the element. The order of elements should
not be changed.
λ> compress "aaaabccaadeeee"
-}
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = myReverse . snd $ foldl' f (x, [x]) xs
    where f (y, ys) z
            | y == z = (z, ys)
            | otherwise = (z, z:ys)

{-
PROBLEM 9
(**) Pack consecutive duplocates of list elements into sublists.
If a list contains repeated elements, they should be placed in 
different sublists

λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
["aaaa", "b", "cc", "aa", "d", "eeee"]
-}
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

{-
PROBLEM 10
(*) Run-length encoding of a list. 
encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: Eq a => [a] -> [(Int, a)]
encode = map encode' . pack
    where encode' x = (length x, head x)