data Occurence a = Single a | Multiple Int a deriving (Show)

decodeModified :: [Occurence a] -> [a]
decodeModified = foldr (\x acc -> replicate' x ++ acc) []
    where replicate' (Single a) = [a]
          replicate' (Multiple x a) = replicate x a
