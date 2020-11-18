repli :: [a] -> Int -> [a]
repli xs y = foldr (\x acc -> repli' x y ++ acc) [] xs
    where repli' a 1 = [a]
          repli' a b = a : repli' a (b - 1) 