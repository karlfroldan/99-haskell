compress :: (Eq a) => [a] -> [a] 
compress (x:xs) = foldl (\acc y -> if last acc /= y
                                   then acc ++ [y]
                                   else acc) [x] xs