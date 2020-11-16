elementAt :: [a] -> Int -> a
elementAt xs y = head $ foldl (\acc x -> if length acc < y 
                                         then x : acc
                                         else acc) [] xs