data Occurence a = Single a | Multiple Int a deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack xs = foldr (\x acc -> if (head . head) acc == x
                           then (x : head acc) : tail acc
                           else [x] : acc) [[last xs]] (init xs)

encodeModified :: (Eq a) => [a] -> [Occurence a]
encodeModified = map repack . pack
    where repack xs = if length xs == 1
                      then Single (head xs)
                      else Multiple (length xs) (head xs)
