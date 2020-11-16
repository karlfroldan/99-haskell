pack :: (Eq a) => [a] -> [[a]]
pack xs = foldr (\x acc -> if (head . head) acc == x
                           then (x : head acc) : tail acc
                           else [x] : acc) [[last xs]] (init xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map repack . pack
    where repack xs = (length xs, head xs)