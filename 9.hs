pack :: (Eq a) => [a] -> [[a]]
pack xs = foldr (\x acc -> if (head . head) acc == x
                           then (x : head acc) : tail acc
                           else [x] : acc) [[last xs]] (init xs)