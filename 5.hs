myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []